use nxt_fsm::*;

pub struct Context {
	pub passwords: Vec<String>,
	pub failed_retries: u32,
}

impl Context {
	pub fn pass(&mut self, user_input: &str) -> bool {
		if self.failed_retries > 2 {
			return false;
		}

		let is_passed = self.passwords.iter().find(|pass| *pass == user_input).is_some();
		if !is_passed {
			self.failed_retries += 1;
		}
		is_passed
	}
}

impl<T: Into<String>> FromIterator<T> for Context {
	fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
		let passwords = iter.into_iter().map(Into::into).collect();
		Self { passwords, failed_retries: 0 }
	}
}

state_machine! {
	#[state_machine(context(crate::Context))]
	#[derive(Debug, PartialEq, Eq, Clone)]
	door(Closed)

	Open(Close) => Closed,
	Closed => {
		Password(pass: String) if context.pass(&pass) => Open else => Closed,
	}
}

impl From<&str> for Input {
	fn from(s: &str) -> Self {
		Input::Password(s.into())
	}
}

use door::{
	Input::{self, Close},
	State::{self, Closed, Open},
};
use test_case::test_case;

#[test_case(["admin"] => Ok(Open))]
#[test_case(["_1","admin"] => Ok(Open))]
#[test_case(["_1", "_2","admin"] => Ok(Open))]
#[test_case(["_1", "_2", "_3", "admin"] => Ok(Closed))]
#[test_case( Vec::<Input>::new() => Ok(Closed) )]
#[test_case( [Input::from("admin"), Close] => Ok(Closed) )]
/*
 */
fn test_door<I, T>(user_inputs: I) -> Result<State, Input>
where
	I: IntoIterator<Item = T>,
	T: Into<Input>,
{
	let context = ["admin", "12345"].into_iter().collect::<Context>();
	let mut sm = door::StateMachine::new(door::State::Closed, context);

	for input in user_inputs.into_iter().map(Into::into) {
		sm.dispatch(input)?;
	}

	Ok(sm.state().clone())
}
