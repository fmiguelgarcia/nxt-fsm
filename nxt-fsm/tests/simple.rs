use nxt_fsm::*;

state_machine! {
#[derive(Debug)]
#[repr(C)]
door(Open)

	Open(Key) => Closed [OutClosed],
	Closed(Key) => Open,
	Open(Break) => Broken [OutBroken],
	Closed(Break) => Broken,
}

#[test]
fn simple() {
	let mut machine = door::StateMachine::new();
	machine.dispatch(door::Input::Key).unwrap();
	println!("{:?}", machine.state());
	machine.dispatch(door::Input::Key).unwrap();
	println!("{:?}", machine.state());
	machine.dispatch(door::Input::Break).unwrap();
	println!("{:?}", machine.state());
}
