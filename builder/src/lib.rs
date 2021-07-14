extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

#[proc_macro_derive(Builder)]
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

    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! { #name: std::option::Option<#ty> }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        quote! { 
            pub fn #name(&mut self, #name: #ty) -> &mut Self {
                self.#name = Some(#name); self
            }
        }
    });

    let assignments = fields.iter().map(|f| {
        let name = &f.ident;
        let msg = format!("Field '{}' is not set", name.clone().unwrap().to_string());
        quote! {
            #name: self.#name.clone().ok_or(#msg)?
        }
    });

    let expanded = quote!{

        #[derive(Default)]
        pub struct #bident{ #(#optionized,)* }

        impl #name{
            pub fn builder() -> #bident {
                #bident{
                    ..Default::default()
                }
            }
        }

        impl #bident { #(#methods)* }

        impl #bident {
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name { #(#assignments,)*})
            }
        }

    };
    expanded.into()
}
