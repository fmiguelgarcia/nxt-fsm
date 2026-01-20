use crate::{transition::SubTransition, Event, Output, UsedTypes};

use proc_macro2::TokenStream;
use quote::quote;
use std::collections::BTreeSet;
use syn::{
	braced,
	parse::{Parse, ParseStream, Result},
	token::Comma,
	Expr, Ident, Token,
};

pub struct MultipleTransition {
	pub match_expr: Box<Expr>,
	pub sub_transitions: Vec<SubTransition>,
}

impl MultipleTransition {
	pub fn to_token_stream(&self, state: &Ident, event: &Event) -> TokenStream {
		let event = event.transition_case();
		let match_expr = &self.match_expr;

		let cases: Vec<TokenStream> = self
			.sub_transitions
			.iter()
			.map(|sub| {
				let pattern = &sub.pattern;
				let next_state = &sub.next_state;
				let output = Output::to_tokens(&sub.output);

				quote! { #pattern => Some( (Self::State::#next_state, #output) ) }
			})
			.collect();

		quote! {
			(
				Self::State::#state, Self::Input::#event) => match #match_expr {
					#(#cases),*
				}
		}
	}
}

impl Parse for MultipleTransition {
	fn parse(input: ParseStream) -> Result<Self> {
		let _ = input.parse::<Token![match]>()?;
		let match_expr = Box::new(Expr::parse_without_eager_brace(input)?);

		let content;
		braced!(content in input);
		let sub_transitions = content.parse_terminated(SubTransition::parse, Comma)?;
		let sub_transitions = sub_transitions.into_iter().collect();

		Ok(Self { match_expr, sub_transitions })
	}
}

impl UsedTypes for MultipleTransition {
	fn outputs(&self) -> BTreeSet<&Ident> {
		self.sub_transitions.iter().flat_map(SubTransition::outputs).collect()
	}

	fn states(&self) -> BTreeSet<&Ident> {
		self.sub_transitions.iter().flat_map(SubTransition::states).collect()
	}
}
