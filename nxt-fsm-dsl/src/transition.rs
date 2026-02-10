mod multiple_transition;
mod single_transition;
mod sub_transition;

use multiple_transition::MultipleTransition;
use single_transition::SingleTransition;
pub(crate) use sub_transition::SubTransition;

use crate::{Event, UsedTypes};

use proc_macro2::TokenStream;
use quote::ToTokens;
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

impl From<SingleTransition> for TransitionMode {
	fn from(st: SingleTransition) -> Self {
		Self::Single(Box::new(st))
	}
}

impl From<MultipleTransition> for TransitionMode {
	fn from(mt: MultipleTransition) -> Self {
		Self::Multiple(mt)
	}
}

/// Represents a part of state transition without the initial state. The `Parse`
/// trait is implemented for the compact form.
pub struct Transition {
	pub parent_state: Option<Ident>,
	pub event: Event,
	mode: TransitionMode,
}

impl Transition {
	#[cfg(feature = "diagram")]
	pub fn diagram(&self) -> Vec<String> {
		use crate::diagram::sanitize_expr;

		let state = self.parent_state.as_ref().expect(TN_PARENT_STATE_EXP);
		match &self.mode {
			TransitionMode::Single(stn) => {
				use crate::transition::single_transition::Guard;

				let guard = stn
					.guard
					.as_ref()
					.map(|g| match g {
						Guard::Expr(expr) => sanitize_expr(expr),
						Guard::Closure(closure) => format!("{}", quote::quote!(#closure)),
					})
					.unwrap_or_default();
				let next_state = &stn.next_state;

				let mut lines = vec![];

				// If there's an else clause, show both branches
				if let Some(else_clause) = &stn.else_clause {
					let if_line = format!("///    {state} --> {next_state}: {} [if {guard}]\n", self.event.name);
					let else_next_state = &else_clause.next_state;
					let else_line = format!("///    {state} --> {else_next_state}: {} [else]\n", self.event.name);
					lines.push(if_line);
					lines.push(else_line);
				} else {
					let diagram_line = format!("///    {state} --> {next_state}: {} {guard}\n", self.event.name);
					lines.push(diagram_line);
				}

				lines
			},
			TransitionMode::Multiple(mtn) => mtn
				.sub_transitions
				.iter()
				.map(|sub| {
					let next_state = &sub.next_state;
					let guard = sanitize_expr(&sub.pattern);
					format!("///    {state} --> {next_state}: {} {guard}\n", self.event.name)
				})
				.collect(),
		}
	}
}

impl ToTokens for Transition {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		let state = self.parent_state.as_ref().expect(TN_PARENT_STATE_EXP);
		let code = match &self.mode {
			TransitionMode::Single(tn) => tn.to_token_stream(state, &self.event),
			TransitionMode::Multiple(mtn) => mtn.to_token_stream(state, &self.event),
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

		let mode: TransitionMode = if input.peek(Token![match]) {
			input.parse::<MultipleTransition>()?.into()
		} else {
			input.parse::<SingleTransition>()?.into()
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

static TN_PARENT_STATE_EXP: &str = "Transition is always in a State .qed";
