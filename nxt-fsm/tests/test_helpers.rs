use nxt_fsm::{StateMachine, StateMachineImpl};
use std::fmt::Debug;

pub fn state_machine_proc<'i, I, O, S, E, SM, II, IO, IS>(
	machine: &mut StateMachine<SM>,
	inputs: II,
	exp_outputs: IO,
	exp_states: IS,
) -> Result<(), E>
where
	SM: StateMachineImpl<Input<'i> = I, Output = O, State = S, Error<'i> = E>,
	S: Clone + PartialEq + Debug,
	O: PartialEq + Debug,
	II: IntoIterator<Item = I>,
	IO: IntoIterator<Item = Option<O>>,
	IS: IntoIterator<Item = S>,
{
	let (states, outputs): (Vec<_>, Vec<_>) = inputs
		.into_iter()
		.map(|input| {
			let output = machine.dispatch(input)?;
			let state = machine.state().clone();
			Ok((state, output))
		})
		.collect::<Result<Vec<_>, _>>()?
		.into_iter()
		.unzip();

	assert_eq!(states, exp_states.into_iter().collect::<Vec<_>>());
	assert_eq!(outputs, exp_outputs.into_iter().collect::<Vec<_>>());
	Ok(())
}
