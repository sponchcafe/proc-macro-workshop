extern crate proc_macro;
extern crate proc_macro2;
use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::spanned::Spanned;
use syn::{parse_macro_input, DeriveInput, Ident};

/// Extract a type `inner` from a type type `ty` like `wrapper<inner>`.
fn ty_inner_type<'a>(ty: &'a syn::Type, wrapper: &'_ str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            // Type is not something like Option<T>
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            // Now we have an Option<T>, looking at T now:
            if inner_ty.args.len() != 1 {
                return None;
            }
            // Now T is a single type argument
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

/// Generate default value for field `f`. Default for regular builder fields is
/// None, for repeated fields it is an empty `Vec`.
fn default_value(f: &syn::Field) -> proc_macro2::TokenStream {
    let name = &f.ident;
    match builder_of(f) {
        Some(_) => quote! { #name: vec![] },
        _ => quote! { #name: None },
    }
}

/// Apply option wrapper to field `f`.The option wrapper is only applied to
/// regular fields. Optional fields are stored as their original (already)
/// Option<T> type. Repeatable fields are stored as `Vec`.
fn optionize_field(f: &syn::Field) -> proc_macro2::TokenStream {
    let name = &f.ident;
    let ty = &f.ty;
    match (builder_of(f), ty_inner_type(ty, "Option")) {
        (Some(_), _) | (_, Some(_)) => quote! { #name: #ty },
        _ => quote! { #name: std::option::Option<#ty> },
    }
}

/// Generate setter for field `f`. Regular builder types are set as a Some.
/// Optional and repeatable types are set as is.
fn field_setter(f: &syn::Field) -> proc_macro2::TokenStream {
    let name = &f.ident;
    let ty = &f.ty;

    let init = match builder_of(f) {
        Some(_) => quote! { #name },
        _ => quote! { Some(#name) },
    };

    let ty = match ty_inner_type(ty, "Option") {
        Some(inner) => quote! { #inner },
        _ => quote! { #ty },
    };
    quote! {
        pub fn #name(&mut self, #name: #ty) -> &mut Self {
            self.#name = #init; self
        }
    }
}

/// Check if a field `f` has the `builder` attribute set on it.
/// If so, returns the containing builder token group `each = "<ident>"`.
fn builder_of(f: &syn::Field) -> Option<proc_macro2::Group> {
    for attr in &f.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
            if let Some(TokenTree::Group(g)) = attr.tokens.clone().into_iter().next() {
                return Some(g);
            }
        }
    }
    None
}

/// Generate field extender for repeateable field `f`.
/// Checks the builder attribute pattern `each = "<ident>"`. If the field is a
/// repeatanble field, returns an extension method. Also returns a boolean
/// indicating if the extension method collides with the regular setter.
fn field_extender(f: &syn::Field) -> Option<(proc_macro2::TokenStream, bool)> {
    let name = f.ident.as_ref().unwrap();
    let ty = &f.ty;
    if let Some(g) = builder_of(f) {
        let mut attr_tokens = g.stream().into_iter();
        match attr_tokens.next().unwrap() {
            TokenTree::Ident(ref i) => {
                if i != "each" {
                    let span = f.attrs[0].path.span().join(g.span()); // Combine spans from path and group
                    let err = syn::Error::new(span.unwrap(), "expected `builder(each = \"...\")`");
                    return Some((err.into_compile_error(), false));
                }
            }
            tt => panic!("Expected 'each' got {}", tt),
        }
        match attr_tokens.next().unwrap() {
            TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
            tt => panic!("Expected '=' got {}", tt),
        }
        let arg = match attr_tokens.next().unwrap() {
            TokenTree::Literal(l) => l,
            tt => panic!("Expected literal, got {}", tt),
        };
        match syn::Lit::new(arg) {
            syn::Lit::Str(s) => {
                let arg = Ident::new(&s.value(), s.span());
                let ty = ty_inner_type(ty, "Vec").expect("'each'-type needs to be Vec<T>");
                return Some((
                    quote! { pub fn #arg(&mut self, #arg: #ty) -> &mut Self {
                            self.#name.push(#arg);
                            self
                        }
                    },
                    &arg == name,
                ));
            }
            tt => panic!("Expected string got {:?}", tt),
        };
    }
    None
}

/// Generate the assignment of field `f` inside the build method.
fn build_method_assignments(f: &syn::Field) -> proc_macro2::TokenStream {
    let name = &f.ident;
    let ty = &f.ty;
    match (builder_of(f), ty_inner_type(ty, "Option")) {
        (_, Some(_)) | (Some(_), _) => quote! { #name: self.#name.clone() },
        _ => quote! {
            #name: self.#name.clone().ok_or(
                concat!(stringify!(#name), " is not set")
            )?
        },
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = Ident::new(&bname, name.span());

    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!();
    };

    let default_values = fields.iter().map(|f| default_value(&f));
    let optionized = fields.iter().map(|f| optionize_field(&f));
    let setters_extenders = fields.iter().filter_map(|f| {
        let setter = field_setter(&f);
        if let Some((mut extender, conflicts)) = field_extender(&f) {
            if !conflicts {
                extender.extend(setter);
                let methods = extender;
                return Some(methods);
            }
            return Some(extender);
        }
        Some(setter)
    });
    let build_method = fields.iter().map(|f| build_method_assignments(&f));

    let builder_doc = format!("Implements the builder pattern for {}", name);
    let expanded = quote! {

        #[doc = #builder_doc]
        pub struct #bident{ #(#optionized,)* }

        impl #name{
            pub fn builder() -> #bident {
                #bident{
                    #(#default_values,)*
                }
            }
        }

        impl #bident {
            #(#setters_extenders)*
        }

        impl #bident {
            pub fn build(&self) -> ::core::result::Result<#name, ::std::boxed::Box<dyn std::error::Error>> {
                Ok(#name { #(#build_method,)*})
            }
        }

    };

    expanded.into()
}
