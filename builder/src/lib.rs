extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Ident};

fn ty_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != "Option" {
            // Type is not something like Option<T>
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            // Now we have an Option<T>, looking at T now:
            if inner_ty.args.len() != 1 { return None; }
            // Now T is a single type argument
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

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
        if ty_inner_type(ty).is_some() {
            quote! { #name: #ty }
        }
        else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if let Some(inner) = ty_inner_type(ty) {
            quote! { 
                pub fn #name(&mut self, #name: #inner) -> &mut Self {
                    self.#name = Some(#name); self
                }
            }
        }
        else {
            quote! { 
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name); self
                }
            }
        }
    });

    let assignments = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        if ty_inner_type(ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        }
        else
        {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
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
            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name { #(#assignments,)*})
            }
        }

    };

    expanded.into()

}
