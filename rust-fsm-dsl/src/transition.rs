mod multiple_transition;
mod single_transition;
mod sub_transition;

use multiple_transition::MultipleTransition;
use single_transition::SingleTransition;
pub(crate) use sub_transition::SubTransition;

use crate::{Event, UsedTypes};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{collections::BTreeSet, iter::once};
use syn::{
	parenthesized,
	parse::{Parse, ParseStream, Result},
	token::Paren,
	Ident, Token,
};

pub enum TransitionMode {
	Single(Box<SingleTransition>),
	Multiple(MultipleTransition),
}

/// Represents a part of state transition without the initial state. The `Parse`
/// trait is implemented for the compact form.
pub struct Transition {
	pub parent_state: Option<Ident>,
	pub event: Event,
	mode: TransitionMode,
}

impl ToTokens for Transition {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let state = self.parent_state.as_ref().expect("Transition is always in a State .qed");
		let code = match &self.mode {
			TransitionMode::Single(tn) => {
				let event = self.event.transition_case();
				quote! { (Self::State::#state, Self::Input::#event) #tn, }
			},
			TransitionMode::Multiple(mtn) => {
				let bindings = &mtn.bindings;
				let event = &self.event.name;
				let cases = &mtn.sub_transitions;

				quote! {
					(
						Self::State::#state,
						Self::Input::#event ( #(#bindings),* )
					) => match ( #(#bindings),* ) {
							#(#cases),*
						}
				}
			},
		};

		code.to_tokens(tokens)
	}
}

impl Parse for Transition {
	fn parse(input: ParseStream) -> Result<Self> {
		let event = if input.peek(Paren) {
			let content;
			parenthesized!(content in input);
			content.parse::<Event>()?
		} else {
			input.parse::<Event>()?
		};

		let mode = if input.peek(Token![match]) {
			let mtn = input.parse::<MultipleTransition>()?;
			TransitionMode::Multiple(mtn)
		} else {
			let stn = input.parse::<SingleTransition>()?;
			TransitionMode::Single(Box::new(stn))
		};
		Ok(Self { parent_state: None, event, mode })
	}
}

impl UsedTypes for Transition {
	fn outputs(&self) -> BTreeSet<&Ident> {
		match &self.mode {
			TransitionMode::Single(stn) => stn.outputs(),
			TransitionMode::Multiple(mtn) => mtn.outputs(),
		}
	}

	fn states(&self) -> BTreeSet<&Ident> {
		match &self.mode {
			TransitionMode::Single(stn) => stn.states(),
			TransitionMode::Multiple(mtn) => mtn.states(),
		}
	}

	fn inputs(&self) -> BTreeSet<&Event> {
		once(&self.event).collect()
	}
}
