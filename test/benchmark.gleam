import artifacts/user
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/iterator
import gleam/list
import lamb
import lamb/query
import lamb/query/term

pub fn main() {
  // process.sleep(1000)
  // io.println("Index Table Benchmark:")
  // let table = initialize_indexed_table(500_000, lamb.Set)
  // run_indexed_benchmark(table)

  process.sleep(1000)
  io.println("Bag Table Benchmark:")
  let table = initialize_bagged_table(50_000)
  run_bagged_benchmark(table)
}

fn initialize_indexed_table(
  quantity: Int,
  kind: lamb.Kind,
) -> lamb.Table(Int, user.User) {
  let assert Ok(table) =
    lamb.create(
      name: "users",
      access: lamb.Protected,
      kind: kind,
      registered: False,
    )

  let enums =
    iterator.from_list([0, 1, 2])
    |> iterator.cycle()

  let ids = iterator.range(1, quantity)

  iterator.map2(enums, ids, fn(enum, id) { #(id, enum) })
  |> iterator.each(fn(params) {
    lamb.insert(table, params.0, user.generate(params.1))
  })

  table
}

fn initialize_bagged_table(
  records quantity: Int,
) -> lamb.Table(Group, user.User) {
  let assert Ok(table) =
    lamb.create(
      name: "users",
      access: lamb.Protected,
      kind: lamb.Bag,
      registered: False,
    )

  let enums =
    iterator.from_list([0, 1, 2])
    |> iterator.cycle()

  let ids = iterator.range(1, quantity)

  iterator.map2(enums, ids, fn(enum, id) { #(id, enum) })
  |> iterator.each(fn(params) {
    let assert [group, ..] = list.shuffle([GroupA, GroupB, GroupC])
    lamb.insert(table, group, user.generate(params.1))
  })

  lamb.insert(table, GroupD, user.generate(0))
  lamb.insert(table, GroupD, user.generate(1))
  lamb.insert(table, GroupD, user.generate(2))

  table
}

pub type Group {
  GroupA
  GroupB
  GroupC
  GroupD
}

pub fn query_find_record(index: index) {
  query.new()
  |> query.index(index)
  |> query.record(term.var(1))
  |> query.map(fn(_, _) { term.var(1) })
}

@external(erlang, "Elixir.Lamb.Artifacts.IndexedBenchmark", "run")
fn run_indexed_benchmark(table: lamb.Table(Int, user.User)) -> x

@external(erlang, "Elixir.Lamb.Artifacts.BaggedBenchmark", "run")
fn run_bagged_benchmark(table: lamb.Table(Group, user.User)) -> x

fn dbg(x: x) -> x {
  let _ = ffi_display(x)
  x
}

pub fn random(x: Int) -> Int {
  int.random(x)
}

@external(erlang, "erlang", "display")
fn ffi_display(x: x) -> y
