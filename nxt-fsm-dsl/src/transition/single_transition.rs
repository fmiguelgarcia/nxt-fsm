use crate::{Event, Output, UsedTypes};

use proc_macro2::TokenStream;
use quote::quote;
use std::{collections::BTreeSet, iter::once};
use syn::{
	parse::{Parse, ParseStream, Result},
	token::Bracket,
	Expr, ExprClosure, Ident, Token,
};

pub enum Guard {
	/// Simple expression: `if some_condition`
	Expr(Expr),
	/// Closure with context: `if |ctx: &mut Self::Context| some_condition`
	Closure(ExprClosure),
}

pub struct ElseClause {
	pub next_state: Ident,
	pub output: Option<Output>,
}

pub struct SingleTransition {
	pub guard: Option<Guard>,
	pub next_state: Ident,
	pub output: Option<Output>,
	pub else_clause: Option<ElseClause>,
}

impl SingleTransition {
	pub fn to_token_stream(&self, state: &Ident, event: &Event) -> TokenStream {
		let event_pattern = event.transition_case();
		let output = Output::to_tokens(&self.output);
		let next_state = &self.next_state;

		match (&self.guard, &self.else_clause) {
			(Some(guard), Some(else_clause)) => {
				// `if-else` guard - usa IntoGuardResult para soportar bool y Result<bool, E>
				let else_output = Output::to_tokens(&else_clause.output);
				let else_next = &else_clause.next_state;

				let guard_expr = match guard {
					Guard::Expr(expr) => quote! { #expr },
					Guard::Closure(closure) => quote! { ( #closure )(context) },
				};

				// Si hay campos, necesitamos reconstruir el input en caso de error
				let pattern = quote! { Self::Input::#event_pattern };

				quote! {
					(Self::State::#state, #pattern) => {
						match ( #guard_expr ).into_guard_result() {
							Ok(true) => Ok((Self::State::#next_state, #output)),
							Ok(false) => Ok((Self::State::#else_next, #else_output)),
							Err(e) => Err(e)
						}
					},
				}
			},
			(Some(guard), None) => {
				// `if` guard
				let guard_expr = match guard {
					Guard::Expr(expr) => quote! { #expr },
					Guard::Closure(closure) => quote! { ( #closure )(context) },
				};

				quote! {
					(Self::State::#state, Self::Input::#event_pattern) if ( #guard_expr ) => {
						Ok((Self::State::#next_state, #output))
					},
				}
			},
			(None, None) => {
				quote! {
					(Self::State::#state, Self::Input::#event_pattern) => Ok((Self::State::#next_state, #output)),
				}
			},
			_ => unreachable!("`else` guard witout `if`"),
		}
	}
}

impl Parse for SingleTransition {
	fn parse(input: ParseStream) -> Result<Self> {
		let guard = if input.peek(Token![if]) {
			let _ = input.parse::<Token![if]>()?;

			// Intentar parsear como closure primero
			if input.peek(Token![|]) {
				let closure = input.parse::<ExprClosure>()?;
				Some(Guard::Closure(closure))
			} else {
				// Si no es una closure, parsear como expresión normal
				let expr = input.parse::<Expr>()?;
				Some(Guard::Expr(expr))
			}
		} else {
			None
		};

		let _ = input.parse::<Token![=>]>()?;
		let next_state = input.parse::<Ident>()?;
		let output = input.peek(Bracket).then(|| input.parse::<Output>()).transpose()?;

		// Parse optional else clause
		let else_clause = input
			.peek(Token![else])
			.then(|| {
				let _ = input.parse::<Token![else]>()?;
				let _ = input.parse::<Token![=>]>()?;
				let else_next_state = input.parse::<Ident>()?;
				let else_output = input.peek(Bracket).then(|| input.parse::<Output>()).transpose()?;
				Ok::<ElseClause, syn::Error>(ElseClause { next_state: else_next_state, output: else_output })
			})
			.transpose()?;

		Ok(Self { guard, next_state, output, else_clause })
	}
}

impl UsedTypes for SingleTransition {
	fn outputs(&self) -> BTreeSet<&Ident> {
		let mut outputs = self.output.as_ref().map(&Output::outputs).unwrap_or_default();
		let mut else_outputs = self
			.else_clause
			.as_ref()
			.and_then(|else_clause| else_clause.output.as_ref())
			.map(&Output::outputs)
			.unwrap_or_default();

		outputs.append(&mut else_outputs);
		outputs
	}

	fn states(&self) -> BTreeSet<&Ident> {
		let mut states = once(&self.next_state).collect::<BTreeSet<_>>();
		if let Some(else_clause) = &self.else_clause {
			states.insert(&else_clause.next_state);
		}
		states
	}
}
