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

    quote!(

        #[derive(Clone)]
        pub struct #bident{
            executable: Option<String>,
            args: Option<Vec<String>>,
            env: Option<Vec<String>>,
            current_dir: Option<String>
        }

        impl #name{
            pub fn builder() -> #bident {
                #bident{
                    executable: None,
                    args: None,
                    env: None,
                    current_dir: None,
                }
            }
        }

        impl #bident {
            pub fn executable(&mut self, executable: String) -> &mut Self {
                self.executable = Some(executable);
                self
            }
            pub fn args(&mut self, args: Vec<String>) -> &mut Self {
                self.args = Some(args);
                self
            }
            pub fn env(&mut self, env: Vec<String>) -> &mut Self {
                self.env = Some(env);
                self
            }
            pub fn current_dir(&mut self, current_dir: String) -> &mut Self {
                self.current_dir = Some(current_dir);
                self
            }
            pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                let cloned = self.clone(); // Required to create value from builder without move
                Ok(#name {
                    executable: cloned.executable.ok_or("Executbale is not set")?,
                    args: cloned.args.ok_or("Args is not set")?,
                    env: cloned.env.ok_or("Env is not set")?,
                    current_dir: cloned.current_dir.ok_or("Current dir is not set")?,
                })
            }
        }

    ).into()
}
