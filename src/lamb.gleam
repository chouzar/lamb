import gleam/erlang
import lamb/query.{type Query}
import lamb/table.{type Table}

pub type Partial(record) {
  Records(List(record), Step)
  End(List(record))
}

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
  ffi_select_count(table_id, [query |> query.map(True)])
}

// ------ FFI Helpers ------ //

pub type Step

type TableId =
  erlang.Reference

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
