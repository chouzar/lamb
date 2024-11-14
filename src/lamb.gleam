import gleam/erlang
import gleam/erlang/atom
import lamb/query.{type Query}

// ------ Table API ------ //

// TODO: Think about enabling the heir option.
// TODO: Think about enabling th give away option (which is independent from heir).
// TODO: Make tables of different types. Set, OrderedSet, Bag, Duplicate Bag.

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

// ------ Record API ------ //

// TODO: When inserting, updating, deleting, querying. Check the table type and
//       who is making the request, who the process owner is to validate that the
//       query can be made.
//
//       Check if ets already gives us a useful type to work with.
pub fn insert(table: Table(index, record), index: index, record: record) -> Nil {
  let table_id = table.reference
  let assert True = ffi_insert(table_id, [#(index, record)])

  Nil
}

pub fn erase(
  table: Table(index, record),
  where query: Query(index, record, binding),
) -> Int {
  let table_id = table.reference
  let query = ffi_update_body(query, True)
  ffi_select_delete(table_id, [query])
}

@external(erlang, "ets", "insert")
fn ffi_insert(table: TableId, rows: List(#(index, record))) -> Bool

@external(erlang, "ets", "select_delete")
fn ffi_select_delete(
  table: TableId,
  queries: List(Query(index, record, binding)),
) -> Int

// ------ Query API ------ //

pub type Partial(record) {
  Records(List(record), Step)
  End(List(record))
}

pub type Step

pub fn all(
  from table: Table(index, record),
  where query: Query(index, record, binding),
) -> List(x) {
  let table_id = table.reference
  ffi_select(table_id, [query])
}

pub fn batch(
  from table: Table(index, record),
  by limit: Int,
  where query: Query(index, record, binding),
) -> Partial(x) {
  let table_id = table.reference
  ffi_search_partial(table_id, limit, [query])
}

pub fn continue(step: Step) -> Partial(x) {
  ffi_search_partial_continue(step)
}

pub fn count(
  from table: Table(index, record),
  where query: Query(index, record, binding),
) -> Int {
  let table_id = table.reference
  let query = ffi_update_body(query, True)
  ffi_select_count(table_id, [query])
}

@external(erlang, "ets", "select")
fn ffi_select(
  table: TableId,
  queries: List(Query(index, record, binding)),
) -> List(x)

@external(erlang, "ets", "select_count")
fn ffi_select_count(
  table: TableId,
  queries: List(Query(index, record, binding)),
) -> Int

@external(erlang, "lamb_erlang_ffi", "search")
fn ffi_search_partial(
  table: TableId,
  limit: Int,
  queries: List(Query(index, record, binding)),
) -> Partial(x)

@external(erlang, "lamb_erlang_ffi", "search")
fn ffi_search_partial_continue(step: Step) -> Partial(x)

@external(erlang, "lamb_query_erlang_ffi", "update_body")
fn ffi_update_body(
  query: Query(index, record, binding),
  with shape: shape,
) -> Query(index, record, x)
