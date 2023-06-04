use crate::namelist::{Array, Item, Map};
use crate::NamelistGroup;

pub(crate) struct NamelistFormatter<'a> {
    key_prefix: Vec<&'a str>,
}

impl<'a> NamelistFormatter<'a> {
    pub(crate) fn new() -> Self {
        NamelistFormatter {
            key_prefix: Vec::new(),
        }
    }
    pub(crate) fn fmt_namelist(
        &mut self,
        namelist: &'a NamelistGroup,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        writeln!(formatter, " &{}", namelist.group_name)?;
        self.fmt_map(&namelist.items, formatter)?;
        write!(formatter, " \\")
    }
    fn fmt_key_prefix(&mut self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for p in &self.key_prefix {
            write!(formatter, "{p}%")?;
        }
        Ok(())
    }
    fn fmt_map(
        &mut self,
        kv: &'a Map<String, Item>,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        for (key, item) in kv {
            match item {
                Item::Literal(lit) => {
                    write!(formatter, "  ")?;
                    self.fmt_key_prefix(formatter)?;
                    writeln!(formatter, "{key} = {lit},")?;
                }
                Item::Derived(d) => {
                    self.key_prefix.push(key);
                    self.fmt_map(d, formatter)?;
                    self.key_prefix.pop();
                }
                Item::Array(array) => {
                    write!(formatter, "  ")?;
                    self.fmt_key_prefix(formatter)?;
                    write!(formatter, "{key} = ")?;
                    self.fmt_array(array, formatter)?;
                    write!(formatter, "\n")?;
                }
            }
        }
        Ok(())
    }

    fn fmt_array(
        &mut self,
        array: &'a Array,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match array {
            Array::RepeatedConstant(r, lit) => {
                self.fmt_key_prefix(formatter)?;
                writeln!(formatter, " = {r}*{lit}")?;
            }
            Array::List(list) => {
                for (r, lit) in list {
                    if *r > 1 {
                        write!(formatter, " {r}*{lit},")?;
                    } else {
                        write!(formatter, " {r},")?;
                    }
                }
            }
            Array::Indexed(_map) => {
                unimplemented!()
            }
        }

        Ok(())
    }
}
