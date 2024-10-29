import artifacts/record
import gleam/io
import gleam/iterator
import lamb.{type Table, Private, Set}

pub fn table(name: String, function: fn(Table(index, record)) -> x) -> Nil {
  // Initialization
  let assert Ok(table) = lamb.create_table(Set, Private, name)

  io.print("\n")
  io.print("test: " <> name)

  // setup test
  function(table)

  io.print(" ✅")

  // Cleanup
  lamb.delete_table(table)
}

pub fn users_table(records quantity: Int) -> Table(Int, record.Record) {
  let assert Ok(table) = lamb.create_table(Set, Private, "test_users")

  let enums =
    iterator.from_list([0, 1, 2])
    |> iterator.cycle()

  let ids = iterator.range(1, quantity)

  iterator.map2(enums, ids, record.generate)
  |> iterator.each(fn(record) { lamb.insert(table, [#(record.id, record)]) })

  table
}

pub fn dbg(term: term) -> term {
  term |> display()
  term
}

@external(erlang, "erlang", "display")
fn display(term: term) -> term
