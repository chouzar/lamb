/// Primitives for building queries
import gleam/erlang/atom.{type Atom}
import gleam/int

pub fn tag(name: String) -> Atom {
  atom.create_from_string(name)
}

pub fn any() -> Atom {
  atom.create_from_string("_")
}

pub fn var(position: Int) -> Atom {
  case position {
    n if n >= 0 && n <= 100_000_000 ->
      atom.create_from_string("$" <> int.to_string(n))
    _other -> panic as "can only specify a variable between 0 and 100_000_000"
  }
}

pub fn records() -> Atom {
  atom.create_from_string("$_")
}

pub fn vars() -> Atom {
  atom.create_from_string("$$")
}
