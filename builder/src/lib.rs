extern crate proc_macro;
extern crate proc_macro2;
use proc_macro2::TokenTree;
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

    let extend_methods = fields.iter().filter_map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let attrs = &f.attrs;
        for attr in attrs {
            if attr.path.segments.len() == 1 && attrs[0].path.segments[0].ident == "builder" {
                if let Some(TokenTree::Group(g)) = attr.tokens.clone().into_iter().next() {
                    let mut attr_tokens = g.stream().into_iter();
                    match attr_tokens.next().unwrap() {
                        TokenTree::Ident(ref i) => assert_eq!(i, "each"),
                        tt => panic!("Expected 'each' got {}", tt)
                    } 
                    match attr_tokens.next().unwrap() {
                        TokenTree::Punct(ref p) => assert_eq!(p.as_char(), '='),
                        tt => panic!("Expected '=' got {}", tt)
                    } 
                    let arg = match attr_tokens.next().unwrap() {
                        TokenTree::Literal(l) => l,
                        tt => panic!("Expected literal, got {}", tt)
                    };
                    match syn::Lit::new(arg) {
                        syn::Lit::Str(s) => {
                            let arg = Ident::new(&s.value(), s.span());
                            return Some(quote!{ 
                                pub fn #arg(&mut self, #arg: #ty) -> Self {
                                    if self.#name.is_none() {
                                        self.name = vec![#arg];
                                    } else {
                                        self.name.push(#arg);
                                    }
                                    self
                                } 
                            });
                        }
                        tt => panic!("Expected string got {:?}", tt)
                    };
                }
            }
        }
        Some(quote! { /* ... */ })
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

        impl #bident { 
            #(#methods)* 
            #(#extend_methods)*
        }

        impl #bident {
            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                Ok(#name { #(#assignments,)*})
            }
        }

    };

    expanded.into()

}
