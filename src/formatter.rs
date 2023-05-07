use crate::value::{Map, Value};
use crate::Namelist;

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
        namelist: &'a Namelist,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(formatter, " &{}\n", namelist.group_name)?;
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
        kv: &'a Map<String, Value>,
        formatter: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        for (key, value) in kv {
            match value {
                Value::ConstantList(v) => {
                    write!(formatter, "  ")?; // TODO: indent
                    self.fmt_key_prefix(formatter)?;
                    write!(formatter, "{} = {},\n", key, v)?;
                }
                Value::Derived(d) => {
                    self.key_prefix.push(&key);
                    self.fmt_map(&d, formatter)?;
                    self.key_prefix.pop();
                }
            }
        }
        Ok(())
    }
}
