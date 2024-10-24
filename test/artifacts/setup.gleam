import artifacts/record
import gleam/io
import gleam/iterator
import lamb.{type Table, Private, Protected}

pub fn table(name: String, function: fn(Table(index, record)) -> x) -> Nil {
  // Initialization
  let assert Ok(table) = lamb.create_table(Private, name)

  io.print("\n")
  io.print("test: " <> name)

  // setup test
  function(table)

  io.print(" ✅")

  // Cleanup
  lamb.delete_table(table)
}

pub fn users_table(records quantity: Int) -> Table(Int, record.Record) {
  let assert Ok(table) = lamb.create_table(Protected, "test_users")

  let enum =
    iterator.from_list([0, 1, 2])
    |> iterator.cycle()
    |> iterator.take(quantity)

  let id = iterator.range(0, quantity)

  iterator.zip(enum, id)
  |> iterator.each(fn(x) {
    let #(enum, id) = x
    let record = record.generate(enum, id)
    lamb.insert(table, id, record)
  })

  table
}
