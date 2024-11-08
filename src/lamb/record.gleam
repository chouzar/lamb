import gleam/erlang
import lamb/query.{type Query}
import lamb/table.{type Table}

type TableId =
  erlang.Reference

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
  where query: Query(index, record),
) -> Int {
  let table_id = table.reference
  ffi_select_delete(table_id, [query |> query.map(True)])
}

@external(erlang, "ets", "insert")
fn ffi_insert(table: TableId, rows: List(#(index, record))) -> Bool

@external(erlang, "ets", "select_delete")
fn ffi_select_delete(table: TableId, queries: List(Query(index, record))) -> Int
