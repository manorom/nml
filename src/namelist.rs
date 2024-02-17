use crate::de::{GroupDeserializer, GroupRefDeserializer};
use crate::error::NamelistError;
use crate::formatter::NamelistFormatter;
use crate::parser::Parser;
use crate::ser::GroupSerializer;
use crate::Result;
use std::{collections::BTreeMap, str::FromStr};

mod array;
mod item;
mod literal_constant;

pub type Map<K, V> = std::collections::BTreeMap<K, V>;
pub type MapIter<'a, K, V> = std::collections::btree_map::Iter<'a, K, V>;
pub type MapIntoIter<K, V> = std::collections::btree_map::IntoIter<K, V>;

pub(crate) use array::{Array, ArrayIter, ArrayRefIter, ItemRef};
pub(crate) use item::Item;
pub(crate) use literal_constant::LiteralConstant;

#[derive(Clone, Debug)]
pub struct NamelistGroup {
    pub(crate) group_name: String,
    pub(crate) items: Map<String, Item>,
}

impl NamelistGroup {
    pub fn new(group_name: impl Into<String>, items: Map<String, Item>) -> NamelistGroup {
        NamelistGroup {
            group_name: group_name.into(),
            items,
        }
    }
}

impl std::ops::Deref for NamelistGroup {
    type Target = Map<String, Item>;

    fn deref(&self) -> &Self::Target {
        &self.items
    }
}

impl FromStr for NamelistGroup {
    type Err = NamelistError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Parser::new(s).parse()
    }
}

impl std::fmt::Display for NamelistGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        NamelistFormatter::new().fmt_namelist(self, f)
    }
}

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

    Ok((
        group.group_name.clone(),
        T::deserialize(GroupDeserializer::new(group))?,
    ))
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
    value
        .serialize(GroupSerializer::new(group_name))
        .map(|nml| format!("{nml}"))
}

/// A structure holding a parsed namelist input. This is the interface to use if
/// you have namelist input (file) with more than one namelist groups.
///
/// A namelist input can contain one or more namelist groups, each with a group
/// name (string) and a series of assignments. A namelist input can contain
/// multiple groups with the same name. Groups can be deserialized with serde's
/// `T::deserialize` method into maps or structs.
///
/// # Example
/// ```rust
/// use serde::Deserialize;
/// #[derive(Deserialize, Debug)]
/// struct Simulation {
///     start_time: i32,
///     timesteps: i32
/// }
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
///        &simulation
///          start_time: 0,
///          timesteps: 10
///        /
///        &particle
///         index = 0,
///         position = 0.0, 0.0, 0.0,
///         velocity = 1.0, 0.0, 0.0,
///        /
///        &particle
///         index = 1,
///         position = 1.0, 0.0, 0.0,
///         velocity = -1.0, 0.0, 0.0,
///        /"#;
///
///    let input = NamelistInput::try_from_str(s)?;
///    let mut simulation = None;
///    let mut particles = Vec::new();
///    for group in input.into_iter() {
///        if group.name() == "particle" {
///            let particle = Particle::deserialize(group)?;
///            particles.push(particle);
///        } else if group.name() == "simulation" {
///            simulation = Some(Simulation::deserialize(group)?);
///        }
///    }
///    Ok(())
///}
/// ```
pub struct NamelistInput(BTreeMap<String, Vec<NamelistGroup>>);

impl NamelistInput {
    /// Parse a string into a namelist input containing one or more namelist
    /// groups.
    pub fn try_from_str(input: &str) -> Result<Self> {
        Self::try_from_parser(Parser::new(input))
    }

    fn try_from_parser(parser: Parser) -> Result<Self> {
        let mut map: BTreeMap<String, Vec<NamelistGroup>> = BTreeMap::new();
        for group in parser {
            let group = group?;
            map.entry(group.group_name.clone())
                .and_modify(|n| n.push(group.clone()))
                .or_insert_with(|| vec![group]);
        }

        Ok(NamelistInput(map))
    }

    /// Returns an Iterator over all namelist groups with matching `group_name`
    /// from the input, producing a [`GroupDeserializer`] for each group.
    /// Note that this removes all groups with `group_name` from the
    /// [`NamelistInput`] because [`GroupDeserializer`] takes ownership of the
    /// parsed Namelist group from [`NamelistInput`].
    pub fn take_groups(&mut self, group_name: &str) -> impl Iterator<Item = GroupDeserializer> {
        self.0
            .remove(group_name)
            .into_iter()
            .flatten()
            .map(|namelist_group| GroupDeserializer::new(namelist_group))
    }

    /// Returns an Iterator over all groups with matching `group_name` from the 
    /// input, producing a [`GroupRefDeserializer`] for each group.
    /// The [`GroupRefDeserializer`] holds a reference to the [`NamelistInput`]
    /// instance, which continues to own the parse Namelist group.
    pub fn groups(&self, group_name: &str) -> impl Iterator<Item = GroupRefDeserializer> {
        self.0
            .get(group_name)
            .into_iter()
            .flatten()
            .map(|namelist_group| GroupRefDeserializer::new(namelist_group))
    }
}

impl IntoIterator for NamelistInput {
    type IntoIter = Box<dyn Iterator<Item = GroupDeserializer>>;
    type Item = GroupDeserializer;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(
            self.0
                .into_iter()
                .map(|(_, v)| v.into_iter().map(|group| GroupDeserializer::new(group)))
                .flatten()
        )
    }
}

impl<'a> IntoIterator for &'a NamelistInput {
    type IntoIter = Box<dyn Iterator<Item = GroupRefDeserializer<'a>> + 'a>;
    type Item = GroupRefDeserializer<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Box::new(
            self.0
                .iter()
                .map(|(_, v)| v.iter().map(|group| GroupRefDeserializer::new(group)))
                .flatten()
        )
    }
}
