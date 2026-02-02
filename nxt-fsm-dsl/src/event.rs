use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::cmp::Ordering;
use syn::{
	parenthesized,
	parse::{Parse, ParseStream, Result},
	token::{Comma, Paren},
	Ident, Token, Type,
};

/// Represents a field in an event tuple variant
#[derive(Clone)]
pub struct EventField {
	pub name: Ident,
	pub ty: Type,
}

pub type EventFields = Vec<EventField>;

pub fn event_fields_to_args(fields: &EventFields) -> Vec<&Ident> {
	fields.iter().map(|field| &field.name).collect()
}

/// Represents an input variant, which can be a simple identifier or a tuple variant
pub struct Event {
	pub name: Ident,
	pub fields: EventFields,
}

impl Event {
	pub fn transition_case(&self) -> TokenStream {
		if self.fields.is_empty() {
			self.name.to_token_stream()
		} else {
			let name = &self.name;
			let fields = event_fields_to_args(&self.fields);
			quote! { #name( #(#fields),* ) }.into_token_stream()
		}
	}
}

impl ToTokens for Event {
	fn to_tokens(&self, tokens: &mut TokenStream) {
		if !self.fields.is_empty() {
			let name = &self.name;
			let field_types = self.fields.iter().map(|f| &f.ty);
			quote! { #name ( #(#field_types),* ) }.to_tokens(tokens);
		} else {
			self.name.to_tokens(tokens);
		}
	}
}

impl Parse for EventField {
	fn parse(input: ParseStream) -> Result<Self> {
		let name = input.parse::<Ident>()?;
		let _ = input.parse::<Token![:]>()?;
		let ty = input.parse::<Type>()?;

		Ok(EventField { name, ty })
	}
}

impl Parse for Event {
	/// Parse the identifier and optionally tuple fields (for compact format)
	fn parse(input: ParseStream) -> Result<Self> {
		let name = input.parse()?;
		let fields = input
			.peek(Paren)
			.then(|| {
				let content;
				parenthesized!(content in input);
				let fields = content.parse_terminated(EventField::parse, Comma)?;
				Ok(fields.into_iter().collect::<Vec<_>>())
			})
			.transpose()?
			.unwrap_or_default();

		Ok(Self { name, fields })
	}
}

impl PartialEq for Event {
	fn eq(&self, other: &Self) -> bool {
		self.name.eq(&other.name)
	}
}

impl Eq for Event {}

impl PartialOrd for Event {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for Event {
	fn cmp(&self, other: &Self) -> Ordering {
		self.name.cmp(&other.name)
	}
}
