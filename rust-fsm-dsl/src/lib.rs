//! DSL implementation for defining finite state machines for `rust-fsm`. See
//! more in the `rust-fsm` crate documentation.

#![recursion_limit = "128"]
extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use std::{collections::BTreeMap, collections::BTreeSet, iter::FromIterator};
use syn::{parse_macro_input, punctuated::Punctuated, token::Comma, Attribute, Ident, Type};

mod parser;

/// The full information about a state transition. Used to unify the
/// represantion of the simple and the compact forms.
struct Transition<'a> {
    initial_state: &'a Ident,
    input_value: &'a parser::InputVariant,
    guard: &'a Option<parser::Guard>,
    final_state: &'a Ident,
    output: &'a Option<parser::OutputSpec>,
}

fn attrs_to_token_stream(attrs: Vec<Attribute>) -> proc_macro2::TokenStream {
    let attrs = attrs.into_iter().map(ToTokens::into_token_stream);
    proc_macro2::TokenStream::from_iter(attrs)
}

#[proc_macro]
/// Produce a state machine definition from the provided `rust-fmt` DSL
/// description.
pub fn state_machine(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as parser::StateMachineDef);

    let doc = attrs_to_token_stream(input.doc);
    let attrs = attrs_to_token_stream(input.attributes);

    if input.transitions.is_empty() {
        let output = quote! {
            compile_error!("rust-fsm: at least one state transition must be provided");
        };
        return output.into();
    }

    let fsm_name = input.name;
    let visibility = input.visibility;

    let transitions = input.transitions.iter().flat_map(|def| {
        def.transitions.iter().map(move |transition| Transition {
            initial_state: &def.initial_state,
            input_value: &transition.input_value,
            guard: &transition.guard,
            final_state: &transition.final_state,
            output: &transition.output,
        })
    });

    let mut states = BTreeSet::new();
    let mut inputs: BTreeMap<&Ident, Option<&Punctuated<Type, Comma>>> = BTreeMap::new();
    let mut outputs = BTreeSet::new();
    let mut transition_cases = Vec::new();
    let mut output_cases = Vec::new();

    #[cfg(feature = "diagram")]
    let mut mermaid_diagram = format!(
        "///```mermaid\n///stateDiagram-v2\n///    [*] --> {}\n",
        input.initial_state
    );

    states.insert(&input.initial_state);

    // Check if we're using a custom input type
    let using_custom_input = input.input_type.is_some();

    for transition in transitions {
        let Transition {
            initial_state,
            final_state,
            input_value,
            guard,
            output,
        } = transition;

        let input_name = &input_value.name;

        #[cfg(feature = "diagram")]
        mermaid_diagram.push_str(&format!(
            "///    {initial_state} --> {final_state}: {input_name}"
        ));

        // Generate match cases
        // For generated input types, we know exactly which pattern to use based on the DSL
        // For custom input types, we only support unit variants (without tuple fields)

        // When there's a guard with tuple variant, we need to bind the fields
        let (input_pattern, guard_expr) = if let Some(guard) = guard {
            if let Some(ref fields) = input_value.fields {
                // Generate parameter names: __arg0, __arg1, etc.
                let param_names: Vec<_> = (0..fields.len())
                    .map(|i| {
                        syn::Ident::new(&format!("__arg{}", i), proc_macro2::Span::call_site())
                    })
                    .collect();

                let guard_expr = &guard.expr;

                // Pattern with named parameters
                let pattern = quote! { Self::Input::#input_name(#(ref #param_names),*) };

                // Call the closure with the bound parameters
                let guard_call = quote! { if (#guard_expr)(#(#param_names),*) };
                (pattern, Some(guard_call))
            } else {
                // Unit variant with guard (guard applies to the state/input combination)
                let guard_expr = &guard.expr;
                (
                    quote! { Self::Input::#input_name },
                    Some(quote! { if #guard_expr }),
                )
            }
        } else {
            // No guard
            let pattern = if using_custom_input || input_value.fields.is_none() {
                // For custom types or unit variants
                quote! { Self::Input::#input_name }
            } else {
                // For generated tuple variants without guard, use (..) pattern
                quote! { Self::Input::#input_name(..) }
            };
            (pattern, None)
        };

        if let Some(guard_clause) = &guard_expr {
            transition_cases.push(quote! {
                (Self::State::#initial_state, #input_pattern) #guard_clause => {
                    Some(Self::State::#final_state)
                }
            });
        } else {
            transition_cases.push(quote! {
                (Self::State::#initial_state, #input_pattern) => {
                    Some(Self::State::#final_state)
                }
            });
        }

        if let Some(output_spec) = output {
            match output_spec {
                parser::OutputSpec::Constant(output_value) => {
                    if let Some(guard_clause) = &guard_expr {
                        output_cases.push(quote! {
                            (Self::State::#initial_state, #input_pattern) #guard_clause => {
                                Some(Self::Output::#output_value)
                            }
                        });
                    } else {
                        output_cases.push(quote! {
                            (Self::State::#initial_state, #input_pattern) => {
                                Some(Self::Output::#output_value)
                            }
                        });
                    }

                    #[cfg(feature = "diagram")]
                    mermaid_diagram.push_str(&format!(" [{output_value}]"));
                }
                parser::OutputSpec::Call(call_expr) => {
                    // Generate code to call the closure with input tuple fields
                    if let Some(ref fields) = input_value.fields {
                        // Generate parameter names: __arg0, __arg1, etc.
                        let param_names: Vec<_> = (0..fields.len())
                            .map(|i| {
                                syn::Ident::new(
                                    &format!("__arg{}", i),
                                    proc_macro2::Span::call_site(),
                                )
                            })
                            .collect();

                        // Pattern to destructure the input with references
                        let pattern_for_call =
                            quote! { Self::Input::#input_name(#(ref #param_names),*) };

                        if let Some(guard_clause) = &guard_expr {
                            output_cases.push(quote! {
                                (Self::State::#initial_state, #pattern_for_call) #guard_clause => {
                                    Some((#call_expr)(#(#param_names),*))
                                }
                            });
                        } else {
                            output_cases.push(quote! {
                                (Self::State::#initial_state, #pattern_for_call) => {
                                    Some((#call_expr)(#(#param_names),*))
                                }
                            });
                        }
                    } else {
                        // Unit variant - call closure without arguments
                        if let Some(guard_clause) = &guard_expr {
                            output_cases.push(quote! {
                                (Self::State::#initial_state, #input_pattern) #guard_clause => {
                                    Some((#call_expr)())
                                }
                            });
                        } else {
                            output_cases.push(quote! {
                                (Self::State::#initial_state, #input_pattern) => {
                                    Some((#call_expr)())
                                }
                            });
                        }
                    }

                    #[cfg(feature = "diagram")]
                    mermaid_diagram.push_str(" [call]");
                }
            }
        }

        #[cfg(feature = "diagram")]
        mermaid_diagram.push('\n');

        states.insert(initial_state);
        states.insert(final_state);

        // Store input variant with its fields
        inputs
            .entry(input_name)
            .or_insert(input_value.fields.as_ref());

        // Only collect constant outputs for the Output enum
        if let Some(parser::OutputSpec::Constant(ref output_ident)) = output {
            outputs.insert(output_ident);
        }
    }

    #[cfg(feature = "diagram")]
    mermaid_diagram.push_str("///```");
    #[cfg(feature = "diagram")]
    let mermaid_diagram: proc_macro2::TokenStream = mermaid_diagram.parse().unwrap();

    let initial_state_name = &input.initial_state;

    // Generate input variants with optional tuple fields
    let input_variants = inputs.iter().map(|(name, fields)| {
        if let Some(fields) = fields {
            quote! { #[allow(unused)] #name(#fields) }
        } else {
            quote! { #[allow(unused)] #name }
        }
    });

    let (input_type, input_impl) = match input.input_type {
        Some(t) => (quote!(#t), quote!()),
        None => (
            quote!(Input),
            quote! {
                #attrs
                pub enum Input {
                    #(#input_variants),*
                }
            },
        ),
    };

    let (state_type, state_impl) = match input.state_type {
        Some(t) => (quote!(#t), quote!()),
        None => (
            quote!(State),
            quote! {
                #attrs
                pub enum State {
                    #(#states),*
                }
            },
        ),
    };

    let (output_type, output_impl) = match input.output_type {
        Some(t) => (quote!(#t), quote!()),
        None => {
            // Many attrs and derives may work incorrectly (or simply not work) for empty enums, so we just skip them
            // altogether if the output alphabet is empty.
            let attrs = if outputs.is_empty() {
                quote!()
            } else {
                attrs.clone()
            };
            (
                quote!(Output),
                quote! {
                    #attrs
                    pub enum Output {
                        #(#outputs),*
                    }
                },
            )
        }
    };

    #[cfg(feature = "diagram")]
    let diagram = quote! {
        #[cfg_attr(doc, ::rust_fsm::aquamarine)]
        #mermaid_diagram
    };

    #[cfg(not(feature = "diagram"))]
    let diagram = quote!();

    // Collect use statements
    let use_statements = &input.use_statements;

    let output = quote! {
        #doc
        #diagram
        #visibility mod #fsm_name {
            #(#use_statements)*

            #attrs
            pub struct Impl;

            pub type StateMachine = ::rust_fsm::StateMachine<Impl>;

            #input_impl
            #state_impl
            #output_impl

            impl ::rust_fsm::StateMachineImpl for Impl {
                type Input = #input_type;
                type State = #state_type;
                type Output = #output_type;
                const INITIAL_STATE: Self::State = Self::State::#initial_state_name;

                fn transition(state: &Self::State, input: &Self::Input) -> Option<Self::State> {
                    match (state, input) {
                        #(#transition_cases)*
                        _ => None,
                    }
                }

                fn output(state: &Self::State, input: &Self::Input) -> Option<Self::Output> {
                    match (state, input) {
                        #(#output_cases)*
                        _ => None,
                    }
                }
            }
        }
    };

    output.into()
}
