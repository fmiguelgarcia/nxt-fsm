mod test_helpers;

use nxt_fsm::*;
use test_helpers::state_machine_proc;

mod user_system_tests {
	use super::*;
	use test_case::test_case;
	use user_system::{
		Input::{self, UserData},
		Output, State, StateMachine,
	};

	state_machine! {
		#[derive(Clone, Debug, PartialEq)]
		#[allow(unused)]
		user_system(Pending)

		Pending => {
			UserData(name: &'static str, age: u32, premium: bool) match (name, age, premium) {
				(_, 18.., true) => VipUser,
				(_, 18.., _) => RegularUser,
				(_, 0..18, _) => MinorUser [ParentalConsentRequired]
			}
		},
		VipUser(Downgrade) => RegularUser,
		RegularUser(Upgrade) => VipUser,
		MinorUser(Approve) => RegularUser
	}

	#[test_case([UserData("Alice", 25, true)],[None],[State::VipUser] => Ok(()) ; "VIP user")]
	#[test_case([UserData("Bob", 20, false)],[None],[State::RegularUser] => Ok(()) ; "Regular user")]
	#[test_case([UserData("Charlie", 16, false)],[Some(Output::ParentalConsentRequired)],[State::MinorUser] => Ok(()) ; "Minor user" )]
	fn test_guards_multiple_fields<I, O, S>(inputs: I, exp_outputs: O, exp_states: S) -> Result<(), ((), Input)>
	where
		I: IntoIterator<Item = Input>,
		O: IntoIterator<Item = Option<Output>>,
		S: IntoIterator<Item = State>,
	{
		let mut machine = StateMachine::default();
		state_machine_proc(&mut machine, inputs, exp_outputs, exp_states)
	}
}

mod string_parser_tests {
	use super::*;
	use string_parser::{
		Input::{self, Parse, WeightParse},
		Output, State,
	};
	use test_case::test_case;

	// Test with &'static str reference type in input
	state_machine! {
	#[derive(Debug, PartialEq, Clone)]
	#[allow(unused)]
	string_parser(Idle)

	Idle => {
		WeightParse(text: &'static str, weight: u32) match (text, weight) {
			("start", _) => Running,
			("stop", _) => Stopped,
			_ => Idle [UnknownCommand]
		}
	},
	Running => {
		Parse(text: &'static str) if text.starts_with("cmd:") => Running [CommandReceived],
		Parse(text: &'static str) if text == "stop" => Stopped
	},
	Stopped(Reset) => Idle
	}

	#[test_case([WeightParse("start", 10)], [None], [State::Running] => Ok(()); "Start")]
	#[test_case([WeightParse("stop", 10)], [None], [State::Stopped] => Ok(()); "Stop")]
	#[test_case([WeightParse("unknown", 0)], [Some(Output::UnknownCommand)], [State::Idle] => Ok(()); "unknown")]
	#[test_case([WeightParse("start", 10), Parse("cmd:hello"), Parse("stop")], [None, Some(Output::CommandReceived), None], [State::Running, State::Running, State::Stopped] => Ok(()); "Start, Command, Stop")]
	fn tests<I, O, S>(inputs: I, exp_outputs: O, exp_states: S) -> Result<(), ((), Input)>
	where
		I: IntoIterator<Item = Input>,
		O: IntoIterator<Item = Option<Output>>,
		S: IntoIterator<Item = State>,
	{
		let mut machine = string_parser::StateMachine::default();
		state_machine_proc(&mut machine, inputs, exp_outputs, exp_states)
	}
}
