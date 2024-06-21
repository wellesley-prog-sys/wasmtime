// TODO(mbm): declare_id is copied from ISLE crate. move it to a common location?
macro_rules! declare_id {
    (
        $(#[$attr:meta])*
            $name:ident
    ) => {
        $(#[$attr])*
            #[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name(pub usize);
        impl $name {
            /// Get the index of this id.
            pub fn index(self) -> usize {
                self.0
            }
        }
    };
}

pub mod debug;
pub mod expand;
pub mod program;
pub mod reachability;
pub mod spec;
pub mod veri;
