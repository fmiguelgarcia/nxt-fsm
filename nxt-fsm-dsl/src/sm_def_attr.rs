use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{meta::ParseNestedMeta, parenthesized, parse::Result, Attribute, Expr, Path, Type};

pub struct SMDefAttr {
	pub input_type: Option<Path>,
	pub state_type: Option<Path>,
	pub output_type: Option<Path>,
	pub context_type: Type,
	pub before_transition: Option<Expr>,
	pub after_transition: Option<Expr>,
}

impl SMDefAttr {
	pub fn before_transition_to_tokens(&self) -> TokenStream {
		// Generate before_transition implementation if provided
		if let Some(ref expr) = self.before_transition {
			quote! {
			fn before_transition<'__input_lifetime>(state: &Self::State, input: &Self::Input<'__input_lifetime>) {
				(#expr)(state, input)
							}
			}
			.into_token_stream()
		} else {
			TokenStream::new()
		}
	}

	pub fn after_transition_to_tokens(&self) -> TokenStream {
		// Generate after_transition implementation if provided
		if let Some(ref expr) = self.after_transition {
			quote! {
				fn after_transition(
					pre_state: &Self::State,
					state: &Self::State,
					output: Option<&Self::Output>,
				) {
					(#expr)(pre_state, state, output)
				}
			}
		} else {
			TokenStream::new()
		}
	}

	fn load_nested_attr(&mut self, meta: ParseNestedMeta<'_>) -> syn::Result<()> {
		let Some(attr_ident) = meta.path.get_ident() else {
			return Ok(());
		};

		let content;
		parenthesized!(content in meta.input);

		let attr_name = attr_ident.to_string();
		match attr_name.as_str() {
			"input" => {
				self.input_type = Some(content.parse::<Path>()?);
			},
			"state" => {
				self.state_type = Some(content.parse::<Path>()?);
			},
			"output" => {
				self.output_type = Some(content.parse::<Path>()?);
			},
			"context" => {
				self.context_type = content.parse::<Type>()?;
			},
			"before_transition" => {
				self.before_transition = Some(content.parse::<Expr>()?);
			},
			"after_transition" => {
				self.after_transition = Some(content.parse::<Expr>()?);
			},
			_ => {},
		}

		Ok(())
	}
}

impl Default for SMDefAttr {
	fn default() -> Self {
		let context_type: Type = syn::parse_str("()").expect("unit type");

		Self {
			input_type: None,
			state_type: None,
			output_type: None,
			context_type,
			before_transition: None,
			after_transition: None,
		}
	}
}

impl FromIterator<Attribute> for SMDefAttr {
	fn from_iter<T>(iter: T) -> Self
	where
		T: IntoIterator<Item = Attribute>,
	{
		let mut this = SMDefAttr::default();

		let _ = iter
			.into_iter()
			.map(|attr| attr.parse_nested_meta(|meta| this.load_nested_attr(meta)))
			.collect::<Result<Vec<_>>>()
			.unwrap();

		this
	}
}
