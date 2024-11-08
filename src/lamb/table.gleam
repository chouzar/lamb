// TODO: Think about enabling the heir option.
// TODO: Think about enabling th give away option (which is independent from heir).
// TODO: Make tables of different types. Set, OrderedSet, Bag, Duplicate Bag.

import gleam/erlang
import gleam/erlang/atom

pub type Table(index, record) {
  Table(
    reference: TableId,
    name: Name,
    kind: Kind,
    access: Access,
    registered: Bool,
  )
}

type TableId =
  erlang.Reference

type Name =
  atom.Atom

pub type Config {
  Config(name: String, access: Access, kind: Kind, registered: Bool)
}

pub type Kind {
  Set
  Bag
}

// TODO: Private and Protected should probably carry a subject.
//       Or we should validate the owner and access before making a query.
pub type Access {
  Public
  Protected
  Private
}

pub type Error {
  AlreadyRegistered(Name)
}

pub fn create(config: Config) -> Result(Table(index, record), Error) {
  // Parameter docs: https://www.erlang.org/doc/apps/stdlib/ets.html#new/2
  let Config(name, access, kind, registered) = config
  let name = atom.create_from_string(name)

  let create = fn() {
    let table_id = ffi_create_table(name, access, kind, registered)
    Table(table_id, name, kind, access, registered)
  }

  // If table needs registration check for naming collisions.
  case registered, ffi_table_id(name) {
    True, Ok(_table_id) -> Error(AlreadyRegistered(name))
    _registered, _table_id -> Ok(create())
  }
}

pub fn delete(table: Table(index, record)) -> Nil {
  // We need to check if table exists otherwise program will crash.
  case is_alive(table) {
    True -> {
      ffi_delete_table(table.reference)
      Nil
    }

    False -> {
      Nil
    }
  }
}

pub fn from_name(name: String) -> Result(TableId, Nil) {
  case atom.from_string(name) {
    Ok(atom) -> ffi_table_id(atom)
    Error(_error) -> Error(Nil)
  }
}

pub fn is_alive(table: Table(index, record)) -> Bool {
  case ffi_table_id(table.reference) {
    Ok(_table_id) -> True
    Error(_error) -> False
  }
}

@external(erlang, "lamb_erlang_ffi", "table_id")
fn ffi_table_id(table: name_or_ref) -> Result(TableId, Nil)

@external(erlang, "lamb_erlang_ffi", "create_table")
fn ffi_create_table(
  name: Name,
  access: Access,
  kind: Kind,
  registered: Bool,
) -> name_or_ref

// Will run into exception if table does not exist.
@external(erlang, "ets", "delete")
fn ffi_delete_table(table: TableId) -> TableId
