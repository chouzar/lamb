import gleam/erlang
import gleam/erlang/atom
import lamb/query.{type Query}

// ------ Table API ------ //

// TODO: Make tables of different types. Set, OrderedSet, Bag, Duplicate Bag.
// TODO: Think about enabling the heir option.
// TODO: Think about enabling th give away option (which is independent from heir).
pub opaque type Table(index, record) {
  Table(reference: TableId)
}

type TableId =
  erlang.Reference

type Name =
  atom.Atom

pub type Store {
  Set
}

// TODO: Private and Protected should probably carry a subject.
//       Or we should validate the owner and access before making a query.
pub type Access {
  Public
  Protected
  Private
}

pub fn create_table(
  store: Store,
  access: Access,
  name: String,
) -> Result(Table(index, record), Nil) {
  // https://www.erlang.org/doc/apps/stdlib/ets.html#new/2
  let name = atom.create_from_string(name)

  case access {
    Private -> {
      let table_id = ffi_create_table(store, access, name)
      Ok(Table(table_id))
    }

    Protected | Public -> {
      case ffi_table_id(name) {
        Ok(_table_id) -> Error(Nil)
        Error(Nil) -> ffi_create_table(store, access, name) |> from_atom()
      }
    }
  }
}

pub fn delete_table(table: Table(index, record)) -> Nil {
  let Table(table_id) = table

  case is_alive(table) {
    True -> ffi_delete_table(table_id)
    False -> table_id
  }

  Nil
}

pub fn from_name(name: String) -> Result(Table(index, record), Nil) {
  case atom.from_string(name) {
    Ok(atom) -> from_atom(atom)
    Error(_error) -> Error(Nil)
  }
}

fn from_atom(name: Name) -> Result(Table(index, record), Nil) {
  case ffi_table_id(name) {
    Ok(table_id) -> Ok(Table(table_id))
    Error(_error) -> Error(Nil)
  }
}

pub fn is_alive(table: Table(index, record)) -> Bool {
  let Table(table_id) = table

  case ffi_table_id(table_id) {
    Ok(_table_id) -> True
    Error(_error) -> False
  }
}

// ------ Record API ------ //

// TODO: When inserting, updating, deleting, querying. Check the table type and
//       who is making the request, who the process owner is to validate that the
//       query can be made.
//
//       Check if ets already gives us a useful type to work with.
//
// TODO: We can make all these operations work o lists. However the behaviour will
//       need to be modified to guarantee insertion.
//
//       > If the list contains more than one object with matching keys and the table
//       > type is set, one is inserted, which one is not defined. The same holds for
//       > table type ordered_set if the keys compare equal.
pub fn insert(table: Table(index, record), rows: List(#(index, record))) -> Nil {
  let Table(table_id) = table
  let assert True = ffi_insert(table_id, rows)

  Nil
}

pub fn update(
  table: Table(index, record),
  where query: Query(index, record),
) -> Int {
  // TODO: Consider these scenarios for bat tables:
  //
  // https://www.erlang.org/doc/apps/stdlib/ets.html#update_element/4
  //
  // >The function fails with reason badarg in the following situations:
  // - The table type is not set or ordered_set.
  // - The element to update is also the key.

  //
  // https://www.erlang.org/doc/apps/stdlib/ets.html#select_replace/2
  //
  // > For the moment, due to performance and semantic constraints, tables of type bag
  // are not yet supported.
  let Table(table_id) = table
  ffi_update(table_id, [query])
}

pub fn delete(
  table: Table(index, record),
  where query: Query(index, record),
) -> Int {
  let Table(table_id) = table
  ffi_delete(table_id, [query |> query.map(True)])
}

// ------ Query API ------ //

// TODO: For tables of type bag, instead of doing a different module we could
//       just do a runtime check before ther request. I think this could make
//       the API way more ergonomic.
pub fn get(table: Table(index, record), index: index) -> Result(record, Nil) {
  let Table(table_id) = table
  ffi_get(table_id, index)
}

pub fn all(
  from table: Table(index, record),
  where query: Query(index, record),
) -> List(x) {
  let Table(table_id) = table
  ffi_search(table_id, [query])
}

pub type Partial(record) {
  Records(List(record), Step)
  End(List(record))
}

pub fn batch(
  from table: Table(index, record),
  by limit: Int,
  where query: Query(index, record),
) -> Partial(x) {
  let Table(table_id) = table
  ffi_search_partial(table_id, limit, [query])
}

pub fn continue(step: Step) -> Partial(x) {
  ffi_search_partial_continue(step)
}

pub fn count(
  from table: Table(index, record),
  where query: Query(index, record),
) -> Int {
  let Table(table_id) = table
  ffi_count(table_id, [query |> query.map(True)])
}

// ------ FFI Helpers ------ //

pub type Step

@external(erlang, "lamb_erlang_ffi", "table_id")
fn ffi_table_id(table: name_or_ref) -> Result(TableId, Nil)

@external(erlang, "lamb_erlang_ffi", "create_table")
fn ffi_create_table(store: Store, access: Access, name: Name) -> name_or_ref

@external(erlang, "ets", "delete")
fn ffi_delete_table(table: TableId) -> TableId

@external(erlang, "ets", "insert")
fn ffi_insert(table: TableId, rows: List(#(index, record))) -> Bool

@external(erlang, "ets", "select_replace")
fn ffi_update(table: TableId, queries: List(Query(index, record))) -> Int

@external(erlang, "ets", "select_delete")
fn ffi_delete(table: TableId, queries: List(Query(index, record))) -> Int

@external(erlang, "lamb_erlang_ffi", "get")
fn ffi_get(table: TableId, index: index) -> Result(record, Nil)

@external(erlang, "ets", "select")
fn ffi_search(table: TableId, queries: List(Query(index, record))) -> List(x)

@external(erlang, "lamb_erlang_ffi", "search")
fn ffi_search_partial(
  table: TableId,
  limit: Int,
  queries: List(Query(index, record)),
) -> Partial(x)

@external(erlang, "lamb_erlang_ffi", "search")
fn ffi_search_partial_continue(step: Step) -> Partial(x)

@external(erlang, "ets", "select_count")
fn ffi_count(table: TableId, queries: List(Query(index, record))) -> Int
