#![feature(prelude_import)]
#[macro_use]
extern crate std;
#[prelude_import]
use std::prelude::rust_2021::*;
use nxt_fsm::*;
use std::sync::atomic::{AtomicU32, Ordering};
pub enum ValidationError {
    InvalidTransition(result_guard_test::Input),
    None,
    InvalidValue,
    OutOfRange,
}
#[automatically_derived]
impl ::core::fmt::Debug for ValidationError {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        match self {
            ValidationError::InvalidTransition(__self_0) => {
                ::core::fmt::Formatter::debug_tuple_field1_finish(
                    f,
                    "InvalidTransition",
                    &__self_0,
                )
            }
            ValidationError::None => ::core::fmt::Formatter::write_str(f, "None"),
            ValidationError::InvalidValue => {
                ::core::fmt::Formatter::write_str(f, "InvalidValue")
            }
            ValidationError::OutOfRange => {
                ::core::fmt::Formatter::write_str(f, "OutOfRange")
            }
        }
    }
}
#[automatically_derived]
impl ::core::marker::StructuralPartialEq for ValidationError {}
#[automatically_derived]
impl ::core::cmp::PartialEq for ValidationError {
    #[inline]
    fn eq(&self, other: &ValidationError) -> bool {
        let __self_discr = ::core::intrinsics::discriminant_value(self);
        let __arg1_discr = ::core::intrinsics::discriminant_value(other);
        __self_discr == __arg1_discr
            && match (self, other) {
                (
                    ValidationError::InvalidTransition(__self_0),
                    ValidationError::InvalidTransition(__arg1_0),
                ) => __self_0 == __arg1_0,
                _ => true,
            }
    }
}
impl From<result_guard_test::Input> for ValidationError {
    fn from(i: result_guard_test::Input) -> Self {
        Self::InvalidTransition(i)
    }
}
mod result_guard_test {
    pub struct Impl;
    #[automatically_derived]
    impl ::core::fmt::Debug for Impl {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(f, "Impl")
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Impl {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Impl {
        #[inline]
        fn eq(&self, other: &Impl) -> bool {
            true
        }
    }
    pub type StateMachine = ::nxt_fsm::StateMachine<Impl>;
    pub enum Input {
        Check(i32),
        Reset,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for Input {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match self {
                Input::Check(__self_0) => {
                    ::core::fmt::Formatter::debug_tuple_field1_finish(
                        f,
                        "Check",
                        &__self_0,
                    )
                }
                Input::Reset => ::core::fmt::Formatter::write_str(f, "Reset"),
            }
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Input {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Input {
        #[inline]
        fn eq(&self, other: &Input) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
                && match (self, other) {
                    (Input::Check(__self_0), Input::Check(__arg1_0)) => {
                        __self_0 == __arg1_0
                    }
                    _ => true,
                }
        }
    }
    pub enum State {
        High,
        Init,
        Low,
    }
    #[automatically_derived]
    impl ::core::fmt::Debug for State {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            ::core::fmt::Formatter::write_str(
                f,
                match self {
                    State::High => "High",
                    State::Init => "Init",
                    State::Low => "Low",
                },
            )
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for State {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for State {
        #[inline]
        fn eq(&self, other: &State) -> bool {
            let __self_discr = ::core::intrinsics::discriminant_value(self);
            let __arg1_discr = ::core::intrinsics::discriminant_value(other);
            __self_discr == __arg1_discr
        }
    }
    pub enum Output {}
    #[automatically_derived]
    impl ::core::fmt::Debug for Output {
        #[inline]
        fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
            match *self {}
        }
    }
    #[automatically_derived]
    impl ::core::marker::StructuralPartialEq for Output {}
    #[automatically_derived]
    impl ::core::cmp::PartialEq for Output {
        #[inline]
        fn eq(&self, other: &Output) -> bool {
            match *self {}
        }
    }
    impl ::nxt_fsm::StateMachineImpl for Impl {
        type Input<'__lifetime> = Input;
        type State = State;
        type Output = Output;
        type Error<'__lifetime> = crate::ValidationError;
        type Context = ();
        const INITIAL_STATE: Self::State = Self::State::Init;
        fn transition<'__lifetime>(
            context: &mut Self::Context,
            state: &Self::State,
            input: Self::Input<'__lifetime>,
        ) -> Result<(Self::State, Option<Self::Output>), Self::Error<'__lifetime>> {
            use ::nxt_fsm::IntoGuardResult;
            match (state, input) {
                (Self::State::High, Self::Input::Reset) => Ok((Self::State::Init, None)),
                (Self::State::Low, Self::Input::Reset) => Ok((Self::State::Init, None)),
                (
                    Self::State::Low,
                    Self::Input::Check(value),
                ) if (value == 42).into_guard_result()? => Ok((Self::State::High, None)),
                (__state_as_err, __input_as_err) => {
                    Err(Self::Error::from(__input_as_err))
                }
            }
        }
    }
}
extern crate test;
#[rustc_test_marker = "test_result_guard"]
#[doc(hidden)]
pub const test_result_guard: test::TestDescAndFn = test::TestDescAndFn {
    desc: test::TestDesc {
        name: test::StaticTestName("test_result_guard"),
        ignore: false,
        ignore_message: ::core::option::Option::None,
        source_file: "nxt-fsm/tests/dsl_syntax.rs",
        start_line: 143usize,
        start_col: 4usize,
        end_line: 143usize,
        end_col: 21usize,
        compile_fail: false,
        no_run: false,
        should_panic: test::ShouldPanic::No,
        test_type: test::TestType::IntegrationTest,
    },
    testfn: test::StaticTestFn(
        #[coverage(off)]
        || test::assert_test_result(test_result_guard()),
    ),
};
fn test_result_guard() {
    let mut machine = result_guard_test::StateMachine::default();
    machine.dispatch(result_guard_test::Input::Check(75)).unwrap();
    if !#[allow(non_exhaustive_omitted_patterns)]
    match machine.state() {
        result_guard_test::State::High => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(machine.state(), result_guard_test::State::High)",
        )
    }
    machine.dispatch(result_guard_test::Input::Reset).unwrap();
    if !#[allow(non_exhaustive_omitted_patterns)]
    match machine.state() {
        result_guard_test::State::Init => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(machine.state(), result_guard_test::State::Init)",
        )
    }
    machine.dispatch(result_guard_test::Input::Check(25)).unwrap();
    if !#[allow(non_exhaustive_omitted_patterns)]
    match machine.state() {
        result_guard_test::State::Low => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(machine.state(), result_guard_test::State::Low)",
        )
    }
    machine.dispatch(result_guard_test::Input::Reset).unwrap();
    if !#[allow(non_exhaustive_omitted_patterns)]
    match machine.state() {
        result_guard_test::State::Init => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(machine.state(), result_guard_test::State::Init)",
        )
    }
    let res = machine.dispatch(result_guard_test::Input::Check(-5));
    if !#[allow(non_exhaustive_omitted_patterns)]
    match res {
        Err(ValidationError::InvalidValue) => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(res, Err(ValidationError::InvalidValue))",
        )
    }
    if !#[allow(non_exhaustive_omitted_patterns)]
    match machine.state() {
        result_guard_test::State::Init => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(machine.state(), result_guard_test::State::Init)",
        )
    }
    let res = machine.dispatch(result_guard_test::Input::Check(150));
    if !#[allow(non_exhaustive_omitted_patterns)]
    match res {
        Err(ValidationError::OutOfRange) => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(res, Err(ValidationError::OutOfRange))",
        )
    }
    if !#[allow(non_exhaustive_omitted_patterns)]
    match machine.state() {
        result_guard_test::State::Init => true,
        _ => false,
    } {
        ::core::panicking::panic(
            "assertion failed: matches!(machine.state(), result_guard_test::State::Init)",
        )
    }
}
#[rustc_main]
#[coverage(off)]
#[doc(hidden)]
pub fn main() -> () {
    extern crate test;
    test::test_main_static(&[&test_result_guard])
}
