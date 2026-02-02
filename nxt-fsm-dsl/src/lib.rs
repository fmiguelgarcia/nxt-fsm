//! DSL implementation for defining finite state machines for `rust-fsm`. See
//! more in the `rust-fsm` crate documentation.
#![recursion_limit = "128"]
extern crate proc_macro;

mod event;
mod output;
mod sm_def_attr;
mod state_def;
// NOTE: current vesion of `fmt` fails on this mod:
// ```
// error[internal]: left behind trailing whitespace
//   --> nxt-fsm/nxt-fsm-dsl/src/state_machine_def.rs:175:175:46
//     |
// 175 |  (_, input_as_err) => Err(input_as_err),
//     | ^
//     |
//
// warning: rustfmt has failed to format. See previous 1 errors.
// ```
#[rustfmt::skip]
mod state_machine_def;
mod transition;

use event::Event;
use output::Output;
use sm_def_attr::SMDefAttr;
use state_def::StateDef;
use state_machine_def::StateMachineDef;
use transition::Transition;

use proc_macro::TokenStream;
use quote::quote;
use std::collections::BTreeSet;
use syn::{parse_macro_input, Ident};

#[cfg(feature = "diagram")]
mod diagram;

#[proc_macro]
/// Produce a state machine definition from the provided `rust-fmt` DSL
/// description.
pub fn state_machine(tokens: TokenStream) -> TokenStream {
	let sm_def = parse_macro_input!(tokens as StateMachineDef);

	quote! { #sm_def }.into()
}

pub(crate) trait UsedTypes {
	fn outputs(&self) -> BTreeSet<&Ident> {
		BTreeSet::new()
	}

	fn states(&self) -> BTreeSet<&Ident> {
		BTreeSet::new()
	}

	fn inputs(&self) -> BTreeSet<&Event> {
		BTreeSet::new()
	}
}
