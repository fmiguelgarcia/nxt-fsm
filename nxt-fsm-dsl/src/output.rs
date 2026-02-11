use crate::UsedTypes;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::collections::BTreeSet;
use syn::{
	bracketed,
	parse::{Parse, ParseStream, Result},
	Error, Expr, ExprCall, ExprClosure, Ident, Token,
};

/// The output of a state transition
pub enum Output {
	/// A constant output variant (e.g., [SetupTimer])
	Constant(Ident),
	/// A constructor call (e.g., [OneValue(x, y)] → Self::Output::OneValue(x, y))
	Constructor(ExprCall),
	/// A function call output (e.g., [|| compute(x)])
	Call(ExprClosure),
}

impl Output {
	pub fn to_tokens(this: &Option<Self>) -> TokenStream {
		this.as_ref().map(|o| quote! { Some(#o) }).unwrap_or_else(|| quote! { None })
	}
}

impl ToTokens for Output {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		match self {
			Output::Constant(id) => {
				quote! { Self::Output::#id }.to_tokens(tokens);
			},
			Output::Constructor(call) => {
				// Extraer el identificador del constructor y los argumentos
				let func = &call.func;
				let args = &call.args;

				// Si func es un Path simple (Ident), convertir a Self::Output::Ident
				if let syn::Expr::Path(path) = &**func {
					if let Some(ident) = path.path.get_ident() {
						quote! { Self::Output::#ident(#args) }.to_tokens(tokens);
						return;
					}
				}

				// Si no es un path simple, usar la expresión tal cual
				quote! { #call }.to_tokens(tokens);
			},
			Output::Call(closure) => {
				let closure_body = &closure.body;
				quote! { #closure_body }.to_tokens(tokens);
			},
		}
	}
}

impl Parse for Output {
	fn parse(input: ParseStream) -> Result<Self> {
		let content;
		bracketed!(content in input);

		// Check if it starts with a closure (|)
		if content.peek(Token![|]) {
			if !content.peek2(Token![|]) {
				return Err(Error::new(
					content.span(),
					"Only support closures without arguments, please use move ownership ",
				));
			}
			let expr = content.parse::<ExprClosure>()?;
			return Ok(Self::Call(expr));
		}

		// Try to parse as identifier first
		let fork = content.fork();
		if let Ok(ident) = fork.parse::<Ident>() {
			// Check if followed by parenthesis (function call syntax)
			if fork.peek(syn::token::Paren) {
				// Parse as constructor call: OneValue(x, y)
				let call = content.parse::<ExprCall>()?;
				return Ok(Self::Constructor(call));
			} else if fork.is_empty() {
				// Just an identifier: SetupTimer
				let _ = content.parse::<Ident>()?;
				return Ok(Self::Constant(ident));
			}
		}

		// Fallback: try to parse as expression call
		let call = content.parse::<ExprCall>()?;
		Ok(Self::Constructor(call))
	}
}

impl UsedTypes for Output {
	fn outputs(&self) -> BTreeSet<&Ident> {
		match self {
			Output::Constant(id) => Some(id).into_iter().collect(),
			Output::Constructor(call) => match &*call.func {
				Expr::Path(path) => path.path.get_ident().into_iter().collect(),
				_ => BTreeSet::new(),
			},
			_ => BTreeSet::new(),
		}
	}
}
