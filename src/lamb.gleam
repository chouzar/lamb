import gleam/erlang
import gleam/erlang/atom.{type Atom}
import gleam/result.{try}
import lamb/query.{type Query}

// ------ Table API ------ //

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

pub type Kind {
  Set
  Bag
}

pub type Access {
  Public
  Protected
  Private
}

pub type Error {
  AlreadyRegistered(Name)
}

pub fn create(
  name name: String,
  access access: Access,
  kind kind: Kind,
  registered registered: Bool,
) -> Result(Table(index, record), Error) {
  // Parameter docs: https://www.erlang.org/doc/apps/stdlib/ets.html#new/2
  let name = atom.create_from_string(name)

  // If table needs registration check for naming collisions.
  case registered, ffi_table_id(name) {
    True, Ok(_table_id) ->
      AlreadyRegistered(name)
      |> Error()

    _registered, _table_id ->
      ffi_create_table(name, access, kind, registered)
      |> Table(name, kind, access, registered)
      |> Ok()
  }
}

pub fn delete(table: Table(index, record)) -> Nil {
  // We need to perform a check because the function
  // ffi_delete_table will crash if table does not exist.
  case ffi_table_info(table.reference) {
    Ok(_info) -> {
      ffi_delete_table(table.reference)
      Nil
    }

    Error(_error) -> {
      Nil
    }
  }
}

pub fn from_name(name: String) -> Result(Table(index, record), Nil) {
  use name <- try(from_string(name))
  use table_id <- try(ffi_table_id(name))
  use info <- try(ffi_table_info(table_id))
  let #(name, kind, access, registered) = info
  let table = Table(table_id, name, kind, access, registered)
  Ok(table)
}

fn from_string(name: String) -> Result(Atom, Nil) {
  name
  |> atom.from_string()
  |> result.replace_error(Nil)
}

pub fn is_alive(table: Table(index, record)) -> Bool {
  case ffi_table_info(table.reference) {
    Ok(_info) -> True
    Error(_error) -> False
  }
}

@external(erlang, "lamb_erlang_ffi", "table_id")
fn ffi_table_id(name: Atom) -> Result(TableId, Nil)

@external(erlang, "lamb_erlang_ffi", "table_info")
fn ffi_table_info(table: TableId) -> Result(#(Name, Kind, Access, Bool), Nil)

@external(erlang, "lamb_erlang_ffi", "create_table")
fn ffi_create_table(
  name: Name,
  access: Access,
  kind: Kind,
  registered: Bool,
) -> TableId

@external(erlang, "ets", "delete")
fn ffi_delete_table(table: TableId) -> TableId

// ------ Record API ------ //

pub fn insert(table: Table(index, record), index: index, record: record) -> Nil {
  let table_id = table.reference
  let assert True = ffi_insert(table_id, [#(index, record)])

  Nil
}

pub fn remove(table: Table(index, record), where query: Query(a, b, c)) -> Int {
  let table_id = table.reference
  let query = query |> query.map(fn(_, _) { True })
  ffi_select_delete(table_id, [query])
}

@external(erlang, "ets", "insert")
fn ffi_insert(table: TableId, rows: List(#(index, record))) -> Bool

@external(erlang, "ets", "select_delete")
fn ffi_select_delete(table: TableId, queries: List(Query(a, b, c))) -> Int

// ------ Query API ------ //

pub type Partial(record) {
  Records(List(record), Step)
  End(List(record))
}

pub type Step

pub fn search(
  from table: Table(index, record),
  where query: Query(a, b, c),
) -> List(x) {
  ffi_select(table.reference, [query])
}

pub fn batch(
  from table: Table(index, record),
  by limit: Int,
  where query: Query(a, b, c),
) -> Partial(x) {
  ffi_batch(table.reference, limit, [query])
}

pub fn continue(step: Step) -> Partial(x) {
  ffi_continue(step)
}

pub fn count(
  from table: Table(index, record),
  where query: Query(a, b, c),
) -> Int {
  let query = query |> query.map(fn(_, _) { True })
  ffi_select_count(table.reference, [query])
}

@external(erlang, "ets", "select")
fn ffi_select(table: TableId, queries: List(Query(a, b, c))) -> List(x)

@external(erlang, "lamb_erlang_ffi", "batch")
fn ffi_batch(
  table: TableId,
  limit: Int,
  queries: List(Query(a, b, c)),
) -> Partial(x)

@external(erlang, "lamb_erlang_ffi", "continue")
fn ffi_continue(step: Step) -> Partial(x)

@external(erlang, "ets", "select_count")
fn ffi_select_count(table: TableId, queries: List(Query(a, b, Bool))) -> Int
