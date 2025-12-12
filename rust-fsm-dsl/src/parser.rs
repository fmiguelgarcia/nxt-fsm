mod guard;
mod input;
mod match_arm;
mod output;
mod transition;

pub use guard::Guard;
pub use input::{InputFields, InputVariant};
pub use match_arm::MatchArm;
pub use output::{Output, OutputSpec};
pub use transition::TransitionDef;

use syn::{
    meta::ParseNestedMeta,
    parenthesized,
    parse::{Parse, ParseStream, Result},
    Attribute, Expr, Ident, ItemUse, Path, Token, Visibility,
};

#[derive(Default)]
pub struct SMDefAttr {
    pub input_type: Option<Path>,
    pub state_type: Option<Path>,
    pub output_type: Option<Path>,
    pub before_transition: Option<Expr>,
    pub after_transition: Option<Expr>,
}

impl SMDefAttr {
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
            }
            "state" => {
                self.state_type = Some(content.parse::<Path>()?);
            }
            "output" => {
                self.output_type = Some(content.parse::<Path>()?);
            }
            "before_transition" => {
                self.before_transition = Some(content.parse::<Expr>()?);
            }
            "after_transition" => {
                self.after_transition = Some(content.parse::<Expr>()?);
            }
            _ => {}
        }

        Ok(())
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

/// Parses the whole state machine definition in the following form (example):
///
/// ```rust,ignore
/// state_machine! {
///     CircuitBreaker(Closed)
///
///     Closed(Unsuccessful) => Open [SetupTimer],
///     Open(TimerTriggered) => HalfOpen,
///     HalfOpen => {
///         Successful => Closed,
///         Unsuccessful => Open [SetupTimer]
///     },
/// }
/// ```
pub struct StateMachineDef {
    pub doc: Vec<Attribute>,
    /// The visibility modifier (applies to all generated items)
    pub visibility: Visibility,
    pub name: Ident,
    pub initial_state: Ident,
    pub use_statements: Vec<ItemUse>,
    pub transitions: Vec<TransitionDef>,
    pub attributes: Vec<Attribute>,
    pub def_attrs: SMDefAttr,
}

impl Parse for StateMachineDef {
    fn parse(input: ParseStream) -> Result<Self> {
        // Parse attributes: doc, state_machine, and others
        let mut attributes = Attribute::parse_outer(input)?;
        let doc = attributes
            .extract_if(.., |attr| attr.path().is_ident("doc"))
            .collect::<Vec<_>>();
        let sm_attrs = attributes
            .extract_if(.., |attr| attr.path().is_ident("state_machine"))
            .collect::<Vec<_>>();
        let def_attrs = SMDefAttr::from_iter(sm_attrs);

        let visibility = input.parse()?;
        let name = input.parse()?;

        // Parse **initial** state.
        let initial_state_content;
        parenthesized!(initial_state_content in input);
        let initial_state = initial_state_content.parse()?;

        // Parse optional use statements
        let mut use_statements = Vec::new();
        while input.peek(Token![use]) {
            use_statements.push(input.parse()?);
        }

        let transitions = input
            .parse_terminated(TransitionDef::parse, Token![,])?
            .into_iter()
            .collect();

        Ok(Self {
            doc,
            visibility,
            name,
            initial_state,
            use_statements,
            transitions,
            attributes,
            def_attrs,
        })
    }
}
