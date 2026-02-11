/// Test for tuple variant input support in the state_machine! macro
use nxt_fsm::*;

state_machine! {
	turnstile(Locked)

	Locked => {
		Coin(_c: u32) => Unlocked,
		Push => Locked
	},
	Unlocked(Push) => Locked
}

state_machine! {
	#[allow(unused)]
	#[derive(Clone)]
	complex_machine(Start)

	Start => {
		Data(_s: String, _a: u32, _p: bool) => Processing,
		Skip => End
	},
	Processing => {
		Complete => End,
		Retry(_c: u32) => Processing
	},
	End(Reset) => Start
}

#[test]
fn tuple_variant_input() {
	use turnstile::{Input, State, StateMachine};

	let mut machine = StateMachine::default();

	// Initial state should be Locked
	assert!(matches!(machine.state(), &State::Locked));

	// Insert coin (tuple variant with u32 value)
	let res = machine.dispatch(Input::Coin(100));
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::Unlocked));

	// Push through (unit variant)
	let res = machine.dispatch(Input::Push);
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::Locked));

	// Try to push when locked
	let res = machine.dispatch(Input::Push);
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::Locked));

	// Insert different coin amount
	let res = machine.dispatch(Input::Coin(50));
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::Unlocked));
}

#[test]
fn tuple_variant_pattern_matching() {
	// Test that we can pattern match on tuple variants
	let coin_input = turnstile::Input::Coin(100);

	match coin_input {
		turnstile::Input::Coin(amount) => {
			assert_eq!(amount, 100);
		},
		_ => panic!("Expected Coin variant"),
	}
}

#[test]
fn complex_tuple_variants() {
	use complex_machine::{Input, State, StateMachine};

	let mut machine = StateMachine::default();

	// Test multi-field tuple variant
	let res = machine.dispatch(Input::Data("test".to_string(), 42, true));
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::Processing));

	// Test single-field tuple variant
	let res = machine.dispatch(Input::Retry(3));
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::Processing));

	// Test unit variant
	let res = machine.dispatch(Input::Complete);
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::End));

	// Test reset
	let res = machine.dispatch(Input::Reset);
	assert!(res.is_ok());
	assert!(matches!(machine.state(), &State::Start));
}
