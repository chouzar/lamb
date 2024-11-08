import artifacts/user
import gleam/io
import gleam/iterator
import lamb/record
import lamb/table.{type Table, Config, Private, Set}

pub fn table(name: String, function: fn(Table(index, record)) -> x) -> Nil {
  // Initialization
  let assert Ok(table) =
    Config(name: name, access: Private, kind: Set, registered: False)
    |> table.create()

  io.print("\n")
  io.print("test: " <> name)

  // setup test
  function(table)

  io.print(" ✅")

  // Cleanup
  table.delete(table)
}

pub fn users_table(records quantity: Int) -> Table(Int, user.User) {
  let assert Ok(table) =
    Config(name: "test_users", access: Private, kind: Set, registered: False)
    |> table.create()

  let enums =
    iterator.from_list([0, 1, 2])
    |> iterator.cycle()

  let ids = iterator.range(1, quantity)

  iterator.map2(enums, ids, user.generate)
  |> iterator.each(fn(record) { record.insert(table, record.id, record) })

  table
}

pub fn dbg(term: term) -> term {
  term |> display()
  term
}

@external(erlang, "erlang", "display")
fn display(term: term) -> term
