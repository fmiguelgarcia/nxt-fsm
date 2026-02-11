/// A trait to unify guard expressions that return either `bool` or `Result<bool, E>`.
/// This allows state machine guards to seamlessly handle both simple boolean conditions
/// and fallible validations that may produce errors.
///
/// # Examples
///
/// ```rust
/// use nxt_fsm::IntoGuardResult;
///
/// // Simple boolean
/// let simple: bool = true;
/// assert_eq!(simple.into_guard_result(), Ok::<bool, ()>(true));
///
/// // Result<bool, E>
/// let fallible: Result<bool, String> = Ok(false);
/// assert_eq!(fallible.into_guard_result(), Ok(false));
///
/// let error: Result<bool, String> = Err("validation failed".to_string());
/// assert_eq!(error.into_guard_result(), Err("validation failed".to_string()));
/// ```
pub trait IntoGuardResult<E> {
	fn into_guard_result(self) -> Result<bool, E>;
}

impl<E> IntoGuardResult<E> for bool {
	fn into_guard_result(self) -> Result<bool, E> {
		Ok(self)
	}
}

impl<T: Into<bool>, E> IntoGuardResult<E> for Result<T, E> {
	fn into_guard_result(self) -> Result<bool, E> {
		self.map(Into::into)
	}
}
