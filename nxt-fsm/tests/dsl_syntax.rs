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

// Ejemplo de máquina de estados con guards que usan contexto
// Demuestra dos tipos de guards:
// 1. Closure guard: `if |ctx: &mut Self::Context| <expr>` - Puede acceder y modificar el contexto
// 2. Expression guard: `if <expr>` - Expresión normal sin acceso al contexto
state_machine! {
	#[derive(Debug)]
	#[state_machine(context(u32))]
	context_guard_test(Start)

	use super::{COUNT, Ordering};

	// Guard con closure: recibe contexto mutable y puede modificarlo
	Start(Increment) if |ctx: &mut Self::Context| { *ctx += 1; *ctx > 2 } => Above else => Below,
	// Guard con expresión: evaluación directa sin contexto
	Above(Reset) if COUNT.load(Ordering::Relaxed) > 5 => Start else => Below,
	// Transición simple sin guard
	Below(Reset) => Start,
}

#[test]
fn dsl_syntax() {
	/*
	let mut machine = door::StateMachine::default();
	machine.consume(&door::Input::Key).unwrap();
	println!("{:?}", machine.state());
	machine.consume(&door::Input::Key).unwrap();
	println!("{:?}", machine.state());
	machine.consume(&door::Input::Break).unwrap();
	println!("{:?}", machine.state());
	*/
}

#[test]
fn test_closure_guard_with_context() {
	let ctx = 0u32;
	let mut machine = context_guard_test::StateMachine::new(context_guard_test::Impl::INITIAL_STATE, ctx);

	// Primera transición: ctx = 1, no pasa la guard (1 <= 2), va a Below
	machine.dispatch(context_guard_test::Input::Increment).unwrap();
	assert!(matches!(machine.state(), context_guard_test::State::Below));
	assert_eq!(*machine.context(), 1);

	// Reset to Start
	machine.dispatch(context_guard_test::Input::Reset).unwrap();
	assert!(matches!(machine.state(), context_guard_test::State::Start));

	// Segunda transición: ctx = 2, no pasa la guard (2 <= 2), va a Below
	machine.dispatch(context_guard_test::Input::Increment).unwrap();
	assert!(matches!(machine.state(), context_guard_test::State::Below));
	assert_eq!(*machine.context(), 2);

	// Reset to Start
	machine.dispatch(context_guard_test::Input::Reset).unwrap();
	assert!(matches!(machine.state(), context_guard_test::State::Start));

	// Tercera transición: ctx = 3, pasa la guard (3 > 2), va a Above
	machine.dispatch(context_guard_test::Input::Increment).unwrap();
	assert!(matches!(machine.state(), context_guard_test::State::Above));
	assert_eq!(*machine.context(), 3);
}

// Test para verificar guards que devuelven Result<bool, Error>
#[derive(Debug, PartialEq, Default)]
pub enum ValidationError {
	#[default]
	None,
	InvalidValue,
	OutOfRange,
}

state_machine! {
	#[derive(Debug)]
	#[state_machine(context(()), error(crate::ValidationError))]
	result_guard_test(Init)

	use super::ValidationError;

	// Guard con closure que devuelve Result - DEBE tener else para soportar Result
	// Si devuelve Err, se propaga el error inmediatamente
	Init(Check(value: i32)) if |_ctx: &mut Self::Context| {
		if value < 0 { Err(Self::Error::InvalidValue) }
		else if value > 100 { Err(Self::Error::OutOfRange) }
		else { Ok(value > 50) }
	} => High else => Low,
	High(Reset) => Init,
	Low(Reset) => Init,
	// También podemos tener guards sin else que solo devuelven bool
	Low(Check(value: i32)) if value == 42 => High,
}

#[test]
fn test_result_guard() {
	let mut machine = result_guard_test::StateMachine::default();

	// Valor válido alto (> 50) - va a High
	machine.dispatch(result_guard_test::Input::Check(75)).unwrap();
	assert!(matches!(machine.state(), result_guard_test::State::High));

	// Reset
	machine.dispatch(result_guard_test::Input::Reset).unwrap();
	assert!(matches!(machine.state(), result_guard_test::State::Init));

	// Valor válido bajo (<= 50) - va a Low
	machine.dispatch(result_guard_test::Input::Check(25)).unwrap();
	assert!(matches!(machine.state(), result_guard_test::State::Low));

	// Reset
	machine.dispatch(result_guard_test::Input::Reset).unwrap();
	assert!(matches!(machine.state(), result_guard_test::State::Init));

	// Valor inválido (< 0) - devuelve error
	let res = machine.dispatch(result_guard_test::Input::Check(-5));
	assert!(matches!(res, Err((ValidationError::InvalidValue, _))));
	assert!(matches!(machine.state(), result_guard_test::State::Init)); // Estado no cambia

	// Valor fuera de rango (> 100) - devuelve error
	let res = machine.dispatch(result_guard_test::Input::Check(150));
	assert!(matches!(res, Err((ValidationError::OutOfRange, _))));
	assert!(matches!(machine.state(), result_guard_test::State::Init)); // Estado no cambia
}

// Test para verificar que guards normales (bool) siguen funcionando
state_machine! {
	#[derive(Debug)]
	bool_guard_test(Start)

	use super::COUNT;
	use super::Ordering;

	// Guard normal que devuelve bool
	Start(Go(value: u32)) if value > 10 => High else => Low,
	High(Reset) => Start,
	Low(Reset) => Start,
}

#[test]
fn test_bool_guard_still_works() {
	let mut machine = bool_guard_test::StateMachine::default();

	// Valor alto (> 10) - va a High
	machine.dispatch(bool_guard_test::Input::Go(20)).unwrap();
	assert!(matches!(machine.state(), bool_guard_test::State::High));

	// Reset
	machine.dispatch(bool_guard_test::Input::Reset).unwrap();
	assert!(matches!(machine.state(), bool_guard_test::State::Start));

	// Valor bajo (<= 10) - va a Low
	machine.dispatch(bool_guard_test::Input::Go(5)).unwrap();
	assert!(matches!(machine.state(), bool_guard_test::State::Low));
}
