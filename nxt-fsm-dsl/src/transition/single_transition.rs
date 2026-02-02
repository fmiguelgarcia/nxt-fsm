use crate::{Event, Output, UsedTypes};

use proc_macro2::TokenStream;
use quote::quote;
use std::{collections::BTreeSet, iter::once};
use syn::{
	parse::{Parse, ParseStream, Result},
	token::Bracket,
	Expr, Ident, Token,
};

pub struct ElseClause {
	pub next_state: Ident,
	pub output: Option<Output>,
}

pub struct SingleTransition {
	pub guard: Option<Expr>,
	pub next_state: Ident,
	pub output: Option<Output>,
	pub else_clause: Option<ElseClause>,
}

impl SingleTransition {
	pub fn to_token_stream(&self, state: &Ident, event: &Event) -> TokenStream {
		let event = event.transition_case();
		let output = Output::to_tokens(&self.output);
		let next_state = &self.next_state;

		match (&self.guard, &self.else_clause) {
			(Some(guard), Some(else_clause)) => {
				// `if-else` guard
				let else_output = Output::to_tokens(&else_clause.output);
				let else_next = &else_clause.next_state;

				quote! {
					(Self::State::#state, Self::Input::#event) => if ( #guard ) {
						Ok((Self::State::#next_state, #output))
					} else {
						Ok((Self::State::#else_next, #else_output))
					},
				}
			},
			(Some(guard), None) => {
				// `if` guard
				quote! {
					(Self::State::#state, Self::Input::#event) if ( #guard ) => {
						Ok((Self::State::#next_state, #output))
					},
				}
			},
			(None, None) => {
				quote! {
					(Self::State::#state, Self::Input::#event) => Ok((Self::State::#next_state, #output)),
				}
			},
			_ => unreachable!("`else` guard witout `if`"),
		}
	}
}

impl Parse for SingleTransition {
	fn parse(input: ParseStream) -> Result<Self> {
		let guard = input
			.peek(Token![if])
			.then(|| {
				let _ = input.parse::<Token![if]>()?;
				input.parse::<Expr>()
			})
			.transpose()?;

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
