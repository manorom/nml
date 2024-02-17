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
//! To serialize and deserialize a Rust struct as a namelist group, you can:
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
//! To deserialize a namelist input (file), consisting of one or more namelist
//! groups, into Rust structs, you can use the [`NamelistInput`] type:
//!
//! ```rust
//! use serde::Deserialize;
//! #[derive(Deserialize, Debug)]
//! struct Simulation {
//!     start_time: i32,
//!     timesteps: i32
//! }
//!
//! #[derive(Deserialize, Debug)]
//! struct Particle {
//!    index: i32,
//!    position: [f32; 3],
//!    velocity: [f32; 3]
//! }
//!
//! fn main() -> Result<(), nml::NamelistError>{
//!    let s = r#"
//!        &simulation
//!          start_time: 0,
//!          timesteps: 10
//!        /
//!        &particle
//!         index = 0,
//!         position = 0.0, 0.0, 0.0,
//!         velocity = 1.0, 0.0, 0.0,
//!        /
//!        &particle
//!         index = 1,
//!         position = 1.0, 0.0, 0.0,
//!         velocity = -1.0, 0.0, 0.0,
//!        /"#;
//!
//!    let input = NamelistInput::try_from_str(s)?;
//!    let mut simulation = None;
//!    let mut particles = Vec::new();
//!
//!    for group in input.into_iter() {
//!        if group.name() == "particle" {
//!            let particle = Particle::deserialize(group)?;
//!            particles.push(particle);
//!        } else if group.name() == "simulation" {
//!            simulation = Some(Simulation::deserialize(group)?);
//!        }
//!    }
//!    Ok(())
//! }
//! ````
//!
//! As you can see, the [`NamelistInput`] type can parse multiple namelist
//! groups and can be transformed into an iterator which yields one instance of
//! [`GroupDeserializer`] for each namelist group in the input.
//! This type  can be used to deserialize the namelist group into Rust structs
//! or maps.
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

pub use error::NamelistError;
pub type Result<T> = std::result::Result<T, NamelistError>;
pub use de::GroupDeserializer;
pub use de::GroupRefDeserializer;
pub use namelist::group_from_str;
pub use namelist::group_to_string;
pub use namelist::NamelistInput;
