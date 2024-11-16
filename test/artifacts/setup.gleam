import artifacts/user
import gleam/io
import gleam/iterator
import lamb.{type Table, Private, Set}

pub fn table(name: String, function: fn(Table(index, record)) -> x) -> Nil {
  // Initialization
  let assert Ok(table) =
    lamb.create(name: name, access: Private, kind: Set, registered: False)

  io.print("\n")
  io.print("test: " <> name)

  // setup test
  function(table)

  io.print(" ✅")

  // Cleanup
  lamb.delete(table)
}

pub fn users_table(records quantity: Int) -> Table(Int, user.User) {
  let assert Ok(table) =
    lamb.create(
      name: "test_users",
      access: Private,
      kind: Set,
      registered: False,
    )

  let enums =
    iterator.from_list([0, 1, 2])
    |> iterator.cycle()

  let ids = iterator.range(1, quantity)

  iterator.map2(enums, ids, user.generate)
  |> iterator.each(fn(record) { lamb.insert(table, record.id, record) })

  table
}

pub fn dbg(term: term) -> term {
  term |> display()
  term
}

@external(erlang, "erlang", "display")
fn display(term: term) -> term
