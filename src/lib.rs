//! A serde library for Fortran namelist inputs.
//! 
//! Namelists are a Fortran 90 feature for input and output of groups of
//! variables in a key-value assignment format.
//! 
//! ```fortran
//! &particle
//!  timestep = 0,
//!  mass = 1.0
//!  position = 1.0, 1.0, 1.0,
//!  velocity = -1.0, 0.0, 0.0
//! /
//! ```
//! 
//! This namelist group assignes an integer variable `timestep`, a floating
//! point/real variable `masss` and two arrays of reals `position` and
//! `velocity`.
//! Further data types supported by the namelist input format are bool/logical
//! values (assinged with `.TRUE.` or `.FALSE.`) and strings (denoted by either
//! single quotes `'hello'`, or double quotes `"hello"`)
//! 
//! # Usage
//! 
//! ## Namelist Groups
//! 
//! To serialize a Rust struct as a namelist group, you can:
//! 
//! ```rust
//! use serde::{Serialize, Deserialize};
//! 
//! #[derive(Serialize, Deserialize, PartialEq, Debug)]
//! struct Particle {
//!     timestep: i32,
//!     mass: f32,
//!     position: [f32; 3],
//!     velocity: [f32; 3]
//! }
//! 
//! fn main() -> Result<(), nml::NamelistError> {
//!     let p = Particle {
//!         timestep: 0,
//!         mass: 1.0,
//!         position: [0.0, 0.0, 0.0],
//!         velocity: [-1.0, 0.0, 0.0]
//!     };
//! 
//!     let serialized = nml::group_to_string(p)?;
//!     let deserialized = nml::group_from_str(&serialized)?;
//! 
//!     assert_eq!(p, deserialized);
//!     Ok(())
//! }
//! ```
//! 
//! ## Multiple Namelist Groups
//! 
//! # Supported Namelist Syntax
//! 
//! **Supported:**
//! 
//! * All basic data types, *integer*, *logical*, *string*, *real*
//! * Arrays: Both Assigning sequences and individual elements using subscripts
//! * Derived types
//! 
//! **Not Supported**:
//! 
//! * Anything involving the slice operator `:`, for example:
//!   `var(1:3) = 1, 2, 3,`
//! * Assignments of sequences to subscripted variables, i.e. `var(1) = 1, 2, 3`

mod de;
mod error;
mod formatter;
mod namelist;
mod parser;
mod reader;
mod ser;

use std::str::FromStr;

#[doc(hidden)]
use namelist::NamelistGroup;
#[doc(hidden)]
pub use de::GroupDeserializer;
pub use error::NamelistError;
#[doc(hidden)]
pub use ser::GroupSerializer;
pub type Result<T> = std::result::Result<T, NamelistError>;

/// Deserialize a namelist group string into an instance of type `T`.
/// 
/// This function returns the namelist group's group name as well as the
/// deserialized type `T`.
///  As a namelist group is a series of key-value assignments, the type `T` must
/// be a struct, map or similar with elements/fields corresponding to the
/// namelist group's keys.
///
/// # Example
/// 
/// ```rust
/// use serde::Deserialize;
///
/// #[derive(Deserialize, Debug)]
/// struct Particle {
///    index: i32,
///    position: [f32; 3],
///    velocity: [f32; 3]
/// }
/// 
/// fn main() -> Result<(), nml::NamelistError>{
///    let s = r#"
///        &particle
///         index = 0,
///         position = 0.0, 0.0, 0.0,
///         velocity = 1.0, 0.0, 0.0,
///        /"#;
///    let particle: Particle = nml::group_from_str(s)?.1;
///    println!("{:#?}", particle);
///    Ok(())
///}
/// ```
/// 
/// # Errors
/// 
/// This function can fail if the structure of the input does not conform to a
/// group in the fortran namelist format, it has unsupported namelist syntax or
/// if it does not conform to the structure expected by `T`.
/// It will also fail if `T` is not a `struct`, `map` or similar data type that
/// can accomodate the keys and values of the namelist group.
pub fn group_from_str<T>(s: &str) -> Result<(String, T)>
where
    T: serde::de::DeserializeOwned,
{
    let group = NamelistGroup::from_str(s)?;

    Ok((group.group_name.clone(), T::deserialize(GroupDeserializer::new(group))?))
}

/// Serialize an instance of type `T` as a namelist group with the given group
/// name in a string. 
/// 
/// This function takes group name and an instance of type `T` to serialize.
/// Type `T` must be a `struct`, `map` or similar type, suitable to be
/// represented as key-value assignments.
/// 
/// # Errors
/// 
/// This function can fail, if the `T` cannot be represented as a namelist
/// group, for example, because it is not a struct, map, or similar type which
/// can be expressed in key-value-assignments.
pub fn group_to_string<T>(group_name: &str, value: &T) -> Result<String>
where
    T: serde::ser::Serialize,
{
    value.serialize(GroupSerializer::new(group_name))
        .map(|nml| format!("{nml}"))
}
