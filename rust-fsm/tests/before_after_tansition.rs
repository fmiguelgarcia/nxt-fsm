use rust_fsm::*;

use crate::door::{Input, State};

fn log_before_transiton(state: &State, input: &Input) {
    println!("Before transition, from state {state:?} with input {input:?}")
}

state_machine! {
    #[derive(Debug)]
    #[repr(C)]
    #[state_machine(after_transition(crate::log_before_transiton))]
    door(Open)

    Open(Key) => Closed,
    Closed(Key) => Open,
    Open(Break) => Broken,
    Closed(Break) => Broken,
}

#[test]
fn simple() {
    let mut machine = door::StateMachine::new();
    machine.consume(&door::Input::Key).unwrap();
    println!("{:?}", machine.state());
    machine.consume(&door::Input::Key).unwrap();
    println!("{:?}", machine.state());
    machine.consume(&door::Input::Break).unwrap();
    println!("{:?}", machine.state());
}
