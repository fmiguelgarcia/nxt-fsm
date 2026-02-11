use std::{
	collections::BTreeMap,
	sync::{
		atomic::{AtomicU32, Ordering::SeqCst},
		LazyLock,
	},
};

use nxt_fsm::{IntoGuardResult, StateMachineImpl};

#[derive(Debug)]
struct PaymentSystem;

enum Input {
	CashPayment(u32),
	CreditPayment(String, u32),
}

enum State {
	Idle,
	Processing,
	Success,
	Failed,
}
enum Output {}

enum Error {
	InvalidTransition(Input),
	MissingAccount,
	NotEnoughBalance,
}

impl From<Input> for Error {
	fn from(i: Input) -> Self {
		Self::InvalidTransition(i)
	}
}

impl StateMachineImpl for PaymentSystem {
	type Input<'a> = Input;
	type State = State;
	type Output = Output;
	type Error<'a> = Error;
	type Context = ();
	const INITIAL_STATE: Self::State = State::Idle;

	fn transition<'a>(
		_context: &mut Self::Context,
		state: &Self::State,
		input: Self::Input<'a>,
	) -> Result<(Self::State, Option<Self::Output>), Self::Error<'a>> {
		match (state, input) {
			(State::Idle, Input::CashPayment(amount))
				if <bool as IntoGuardResult<Error>>::into_guard_result(amount >= 100)? =>
				Ok((State::Processing, None)),
			(State::Idle, Input::CreditPayment(acc, amount)) if check_credit(&acc, amount).into_guard_result()? =>
				Ok((State::Processing, None)),
			(__state, __input) => Err(Self::Error::from(__input)),
		}
	}
}

fn check_credit(acc: &str, amount: u32) -> Result<bool, Error> {
	static ACCOUNTS: LazyLock<BTreeMap<&'static str, AtomicU32>> = LazyLock::new(|| {
		[("Alice", AtomicU32::new(100)), ("Bob", AtomicU32::new(200)), ("Charlie", AtomicU32::new(50))]
			.into_iter()
			.collect::<BTreeMap<_, _>>()
	});

	let balance = &*ACCOUNTS.get(acc).ok_or(Error::MissingAccount)?;
	balance
		.fetch_update(SeqCst, SeqCst, |balance| balance.checked_sub(amount))
		.map_err(|_| Error::NotEnoughBalance)?;
	Ok(true)
}


