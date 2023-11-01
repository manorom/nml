# nml
Serialize and deserialize Fortran namelist input in Rust using the serde framework.

## Usage

```rust
use serde::Deserialize;

#[derive(Deserialize, Debug)]
struct Particle {
    index: i32,
    position: [f32; 3],
    velocity: [f32; 3]
}

fn main() -> Result<(), nml::NamelistError>{
    let s = r#"
      &particle
       index = 0,
       position = 0.0, 0.0, 0.0,
       velocity = 1.0, 0.0, 0.0,
      /"#;

  let particle: Particle = nml::group_from_str(s)?.1;
  println!("{:#?}", particle);

  Ok(())
}
