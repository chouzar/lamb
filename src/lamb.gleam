import gleam/erlang
import gleam/erlang/atom

// ------ Table API ------ //

pub opaque type Table(index, record) {
  Table(TableId)
}

type TableId =
  erlang.Reference

type Name =
  atom.Atom

pub type Partial(record) {
  Records(List(record), Step)
  End(List(record))
}

pub type Access {
  Public
  Protected
  Private
}

pub fn create_table(
  access: Access,
  name: String,
) -> Result(Table(index, record), Nil) {
  // https://www.erlang.org/doc/apps/stdlib/ets.html#new/2
  let name = atom.create_from_string(name)

  case access {
    Private -> {
      let table_id = ffi_create_table(access, name)
      Ok(Table(table_id))
    }

    Protected | Public -> {
      case ffi_table_id(name) {
        Ok(_table_id) -> Error(Nil)
        Error(Nil) -> ffi_create_table(access, name) |> from_atom()
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

pub fn insert(table: Table(index, record), index: index, record: record) -> Nil {
  let Table(table_id) = table
  let assert True = ffi_insert(table_id, #(index, record))

  Nil
}

pub fn delete(table: Table(index, record), index: index) -> Nil {
  let Table(table_id) = table
  let assert True = ffi_delete(table_id, index)

  Nil
}

// ------ Query API ------ //

pub fn get(table: Table(index, record), index: index) -> Result(record, Nil) {
  let Table(table_id) = table
  ffi_get(table_id, index)
}

pub fn all(table: Table(index, record)) -> List(record) {
  let query = query_record()
  search(table, query)
}

pub fn partial(table: Table(index, record), by limit: Int) -> Partial(record) {
  let query = query_record()
  search_partial(table, limit, query)
}

pub fn count(table: Table(index, record)) -> Int {
  let query = query_count()
  search_count(table, query)
}

pub fn continue(step: Step) -> Partial(x) {
  ffi_search_partial_continue(step)
}

// ------ Query Search API ------ //

pub type MatchFunction

pub fn search(
  table: Table(index, record),
  where expression: List(MatchFunction),
) -> List(x) {
  // Without matchspec ets:tab2list should be faster.
  let Table(table_id) = table
  ffi_search(table_id, expression)
}

pub fn search_partial(
  table: Table(index, record),
  by limit: Int,
  where expression: List(MatchFunction),
) -> Partial(x) {
  let Table(table_id) = table
  ffi_search_partial(table_id, limit, expression)
}

pub fn search_count(
  table: Table(index, record),
  where expression: List(MatchFunction),
) -> Int {
  let Table(table_id) = table
  ffi_count(table_id, expression)
}

// ------ FFI Helpers ------ //

pub type Step

@external(erlang, "lamb_erlang_ffi", "table_id")
fn ffi_table_id(table: name_or_ref) -> Result(TableId, Nil)

@external(erlang, "lamb_erlang_ffi", "create_table")
fn ffi_create_table(access: Access, name: Name) -> name_or_ref

@external(erlang, "ets", "delete")
fn ffi_delete_table(table: TableId) -> TableId

@external(erlang, "ets", "insert")
fn ffi_insert(table: TableId, record: record) -> Bool

@external(erlang, "ets", "delete")
fn ffi_delete(table: TableId, index: index) -> Bool

@external(erlang, "lamb_erlang_ffi", "get")
fn ffi_get(table: TableId, index: index) -> Result(record, Nil)

@external(erlang, "lamb_erlang_ffi", "search")
fn ffi_search(table: TableId, expression: List(MatchFunction)) -> List(record)

@external(erlang, "lamb_erlang_ffi", "search")
fn ffi_search_partial(
  table: TableId,
  limit: Int,
  expression: List(MatchFunction),
) -> Partial(x)

@external(erlang, "lamb_erlang_ffi", "search")
fn ffi_search_partial_continue(step: Step) -> Partial(x)

@external(erlang, "lamb_erlang_ffi", "count")
fn ffi_count(table: TableId, expression: List(MatchFunction)) -> Int

// ------ FFI Queries ------ //

@external(erlang, "lamb_erlang_queries_ffi", "query_count")
fn query_count() -> List(MatchFunction)

@external(erlang, "lamb_erlang_queries_ffi", "query_record")
fn query_record() -> List(MatchFunction)
