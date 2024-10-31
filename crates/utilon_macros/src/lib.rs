use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    token::Comma,
    Ident, Item,
};

struct ActivityArgs {
    activity_name: Ident,
}

impl Parse for ActivityArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let scorer_name: Ident = input.parse()?;
        Ok(ActivityArgs {
            activity_name: scorer_name,
        })
    }
}

#[proc_macro_attribute]
pub fn activity(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as ActivityArgs);
    let input = parse_macro_input!(item as Item);

    let syn::Item::Struct(ref data) = input else {
        panic!("Activity can only be implemented for structs");
    };
    let name = &data.ident;
    let activity_name = args.activity_name;

    let output = quote!(
        #input

        impl Activity for #name {
            fn init(world: &mut bevy::prelude::World) {
                world.init_component::<#name>();
            }

            fn system<const A: ActivityId, S: ActivitySeq>() -> bevy::ecs::schedule::SystemConfigs {
                bevy::prelude::IntoSystemConfigs::into_configs(#activity_name::<A, S>)
            }

            #[inline]
            fn enter(ec: &mut bevy::ecs::system::EntityCommands) {
                ec.insert(#name);
            }

            #[inline]
            fn exit(ec: &mut bevy::ecs::system::EntityCommands) {
                ec.remove::<#name>();
            }
        }
    );

    TokenStream::from(output)
}

struct ActivitySeqArgs {
    idents: Vec<Ident>,
}

impl Parse for ActivitySeqArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut idents = vec![input.parse::<Ident>()?];
        while input.parse::<Comma>().is_ok() {
            idents.push(input.parse::<Ident>()?);
        }
        Ok(ActivitySeqArgs { idents })
    }
}

#[proc_macro]
pub fn impl_activity_seq(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ActivitySeqArgs);

    let type_vars = input.idents;
    let into_activity_list = type_vars.iter().enumerate().map(|(i, ident)| {
        let i = i as u8;
        quote!(
            #ident::system::<#i, Self>()
        )
    });
    let enter = type_vars.iter().enumerate().map(|(i, ident)| {
        let i = i as u8;
        quote!(
            #i => <#ident as Activity>::enter(ec)
        )
    });
    let exit = type_vars.iter().enumerate().map(|(i, ident)| {
        let i = i as u8;
        quote!(
            #i => <#ident as Activity>::exit(ec)
        )
    });
    let type_path_format = format!(
        "({})",
        type_vars
            .iter()
            .map(|_| "{}".to_string())
            .reduce(|a, b| format!("{},{}", a, b))
            .unwrap()
    );
    let type_path = type_vars
        .iter()
        .map(|ident| quote!(<#ident as bevy::reflect::TypePath>::type_path()));
    let short_type_path = type_vars
        .iter()
        .map(|ident| quote!(<#ident as bevy::reflect::TypePath>::short_type_path()));

    let imp = quote!(
        impl<#(#type_vars),*> ActivitySeq for (#(#type_vars),*,)
        where
            #(#type_vars: Activity),* {

            fn init(world: &mut bevy::prelude::World) {
                #(world.init_component::<#type_vars>();)*
            }

            fn into_activity_list() -> ActivityList {
                ActivityList(
                    vec![#(#into_activity_list),*]
                )
            }

            fn enter(index: ActivityId, ec: &mut EntityCommands) {
                match index {
                    #(#enter),*,
                    _ => unreachable!("Invalid activity id")
                };
            }

            fn exit(index: ActivityId, ec: &mut EntityCommands) {
                match index {
                    #(#exit),*,
                    _ => unreachable!("Invalid activity id")
                };
            }

            fn type_path() -> &'static str {
                use bevy::reflect::utility::GenericTypePathCell;
                static CELL: GenericTypePathCell = GenericTypePathCell::new();
                CELL.get_or_insert::<Self, _>(|| format!(#type_path_format, #(#type_path),*))
            }

            fn short_type_path() -> &'static str {
                use bevy::reflect::utility::GenericTypePathCell;
                static CELL: GenericTypePathCell = GenericTypePathCell::new();
                CELL.get_or_insert::<Self, _>(|| format!(#type_path_format, #(#short_type_path),*))
            }
        }
    );

    TokenStream::from(imp)
}
