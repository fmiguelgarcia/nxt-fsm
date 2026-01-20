use nxt_fsm::state_machine;

state_machine! {
	/// A dummy implementation of the Circuit Breaker pattern to demonstrate
	/// capabilities of its library DSL for defining finite state machines.
	/// https://martinfowler.com/bliki/CircuitBreaker.html
	pub circuit_breaker(Closed)

	Closed(Unsuccessful) => Open [SetupTimer],
	Open(TimerTriggered) => HalfOpen,
	HalfOpen => {
		Successful => Closed,
		Unsuccessful => Open [SetupTimer]
	}
}

// Define custom types for the authentication example
#[derive(Debug, PartialEq)]
pub enum AuthOutput {
	AdminMode,
	RegularMode(String),
}

state_machine! {
	#[derive(Debug, PartialEq)]
	#[state_machine(output(AuthOutput))]
	pub auth_system(Unauthenticated)

	use super::{AuthOutput};

	Unauthenticated => {
		PreLogin(age: u32) if age >= 18 => Login else => Restricted,
	},
	Login => {
		Auth(user: String, pass: String) match (user.as_str(), pass.as_str()) {
			("root", _pass) => Authenticated [ AdminMode ],
			(_user, "") => Unauthenticated,
			(user, _pass) => Authenticated [ || AuthOutput::RegularMode(user.to_string())],
		},
	},
	Authenticated(Logout) => Unauthenticated,
}

// Define a custom output type for the calculator
#[derive(Debug, PartialEq)]
pub enum CalcOutput {
	Result(i32),
	Clear,
}

state_machine! {
	#[derive(Debug, PartialEq)]
	#[state_machine(output(CalcOutput))]
	/// A simple calculator state machine demonstrating arithmetic operations and error handling.
	/// It uses Inputs carrying data, like _operands_, and closures to generate output.
	pub calculator(Idle)

	use super::CalcOutput;

	Idle => {
		Add(a: i32, b: i32) => Idle [|| CalcOutput::Result(a + b)],
		Multiply(x: i32, y: i32) => Idle [|| CalcOutput::Result(x * y)],
		Divide(x: i32, y: i32) match (x, y) {
			(_, 0) => ErrDivByZero,
			(x, y) => Idle [ || CalcOutput::Result(x/y)]
		}
	},
	ErrDivByZero(Reset) => Idle [Clear]
}
