use nxt_fsm::*;
use std::sync::atomic::{AtomicU32, Ordering};

static COUNT: AtomicU32 = AtomicU32::new(0);

state_machine! {
	#[derive(Debug)]
	my_sm(Init)

	use super::{COUNT, Ordering};

	// Check simple transitions,
	Init(InputA) => A[OutA],
	A(InputB) => B,
	A(InputC) => C,
	// Check `if` on single transition with named fields.
	A(InputD (a: u32, _b: String)) if a > 100 => D,
	// Check multi transition from B
	B => {
		InputA => A [OutA],
		InputC => C,
		// Check `if` guards with bindings to input using named fields.
		InputD(a: u32, b: String) if a >= 42 && !b.is_empty() => D [OutD],
		// Check `if` guard without bindings
		InputB if COUNT.load(Ordering::Relaxed) > 0 => D,
		// Check `match` guards
		InputE(x: u32, y: u32) match (x,y) {
		  (0..10, _) => A [OutA],
		  (10.., 0..1_000) => B [OutB],
		  (10.., 1_000..1_000_000) => C ,
		  _ => D,
		}
	}
}

pub enum MyOut {
	O1,
	O2(u32),
	O3(u32, String),
}

pub fn inc_o2() -> MyOut {
	MyOut::O2(COUNT.fetch_add(1, Ordering::Relaxed))
}

state_machine! {
	#[derive(Debug)]
	#[state_machine(output(crate::MyOut))]
	check_gen_output(A)

	use super::inc_o2;

	A (E1) => B [ O1 ],
	B (E1) => C,
	B (E2) => C [ || inc_o2() ],
	B (E3(a: u32, b: String)) => A [ || Self::Output::O3(a, b) ]
}

#[test]
fn dsl_syntax() {
	/*
	let mut machine = door::StateMachine::new();
	machine.consume(&door::Input::Key).unwrap();
	println!("{:?}", machine.state());
	machine.consume(&door::Input::Key).unwrap();
	println!("{:?}", machine.state());
	machine.consume(&door::Input::Break).unwrap();
	println!("{:?}", machine.state());
	*/
}
