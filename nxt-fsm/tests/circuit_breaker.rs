/// A dummy implementation of the Circuit Breaker pattern to demonstrate
/// capabilities of this library.
/// https://martinfowler.com/bliki/CircuitBreaker.html
use nxt_fsm::*;
use std::{
	sync::{Arc, Mutex},
	time::Duration,
};

#[derive(Debug)]
enum CircuitBreakerInput {
	Successful,
	Unsuccessful,
	TimerTriggered,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum CircuitBreakerState {
	Closed,
	Open,
	HalfOpen,
}

#[derive(Debug, PartialEq)]
struct CircuitBreakerOutputSetTimer;

#[derive(Debug)]
struct CircuitBreakerMachine;

impl StateMachineImpl for CircuitBreakerMachine {
	type Input<'a> = CircuitBreakerInput;
	type State = CircuitBreakerState;
	type Output = CircuitBreakerOutputSetTimer;
	type Error = ();
	type Context = ();
	const INITIAL_STATE: Self::State = CircuitBreakerState::Closed;

	fn transition<'a>(
		_context: &mut Self::Context,
		state: &Self::State,
		input: Self::Input<'a>,
	) -> Result<(Self::State, Option<Self::Output>), (Self::Error, Self::Input<'a>)> {
		match (state, input) {
			(CircuitBreakerState::Closed, CircuitBreakerInput::Unsuccessful) => {
				let next_state = CircuitBreakerState::Open;
				let output = Some(CircuitBreakerOutputSetTimer);
				Ok((next_state, output))
			},
			(CircuitBreakerState::Open, CircuitBreakerInput::TimerTriggered) =>
				Ok((CircuitBreakerState::HalfOpen, None)),
			(CircuitBreakerState::HalfOpen, CircuitBreakerInput::Successful) => Ok((CircuitBreakerState::Closed, None)),
			(CircuitBreakerState::HalfOpen, CircuitBreakerInput::Unsuccessful) => {
				let next_state = CircuitBreakerState::Open;
				let output = Some(CircuitBreakerOutputSetTimer);
				Ok((next_state, output))
			},
			(_, input_as_err) => Err(((), input_as_err)),
		}
	}
}

#[test]
fn circuit_breaker() {
	let machine: StateMachine<CircuitBreakerMachine> = StateMachine::default();

	// Unsuccessful request
	let machine = Arc::new(Mutex::new(machine));
	{
		let mut lock = machine.lock().unwrap();
		let res = lock.dispatch(CircuitBreakerInput::Unsuccessful).unwrap();
		assert_eq!(res, Some(CircuitBreakerOutputSetTimer));
		assert_eq!(lock.state(), &CircuitBreakerState::Open);
	}

	// Set up a timer
	let machine_wait = machine.clone();
	std::thread::spawn(move || {
		std::thread::sleep(Duration::from_millis(500));
		let mut lock = machine_wait.lock().unwrap();
		let res = lock.dispatch(CircuitBreakerInput::TimerTriggered).unwrap();
		assert_eq!(res, None);
		assert_eq!(lock.state(), &CircuitBreakerState::HalfOpen);
	});

	// Try to pass a request when the circuit breaker is still open
	let machine_try = machine.clone();
	std::thread::spawn(move || {
		std::thread::sleep(Duration::from_millis(100));
		let mut lock = machine_try.lock().unwrap();
		let res = lock.dispatch(CircuitBreakerInput::Successful);
		assert!(matches!(res, Err(((), CircuitBreakerInput::Successful))));
		assert_eq!(lock.state(), &CircuitBreakerState::Open);
	});

	// Test if the circit breaker was actually closed
	std::thread::sleep(Duration::from_millis(700));
	{
		let mut lock = machine.lock().unwrap();
		let res = lock.dispatch(CircuitBreakerInput::Successful).unwrap();
		assert_eq!(res, None);
		assert_eq!(lock.state(), &CircuitBreakerState::Closed);
	}
}
