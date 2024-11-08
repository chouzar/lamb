import artifacts/setup
import artifacts/user.{Admin, User}
import gleam/list
import gleeunit
import lamb.{End, Records}
import lamb/query.{atom, ignore, var} as q
import lamb/record
import lamb/table.{Config, Private, Set}

pub fn main() {
  gleeunit.main()
}

pub fn table_test() {
  // Able to create a private table
  let assert Ok(t0) = table.create(Config("test_table", Private, Set, False))
  let assert True = table.is_alive(t0)

  // Able to create a private table with same name
  let assert Ok(t1) = table.create(Config("test_table", Private, Set, False))
  let assert True = table.is_alive(t1)

  // Able to create a registered table
  let assert Ok(t2) = table.create(Config("test_table", Private, Set, True))
  let assert True = table.is_alive(t2)

  // Unable to create a registered table with same name
  let assert Error(_) = table.create(Config("test_table", Private, Set, True))

  // Able to retrieve a table by name
  let assert Ok(_) = table.from_name("test_table")

  // Able to delete a table
  table.delete(t0)
  let assert False = table.is_alive(t0)

  table.delete(t1)
  let assert False = table.is_alive(t1)

  table.delete(t2)
  let assert False = table.is_alive(t2)
  let assert Error(_) = table.from_name("test_table")
}

pub fn record_test() {
  setup.table("insert records", fn(table) {
    let assert [] = lamb.all(table, q.new())

    record.insert(table, "a", user.random(1))
    let assert [_] = lamb.all(table, q.new())

    record.insert(table, "b", user.random(2))
    let assert [_, _] = lamb.all(table, q.new())

    record.insert(table, "c", user.random(3))
    let assert [_, _, _] = lamb.all(table, q.new())
  })

  setup.table("delete records", fn(table) {
    let assert [] = lamb.all(table, q.new())

    record.insert(table, "a", user.random(1))
    record.insert(table, "b", user.random(2))
    record.insert(table, "c", user.random(3))

    let assert 3 = record.erase(table, q.new())
    let assert [] = lamb.all(table, q.new())
  })
}

pub fn simple_query_test() {
  setup.table("retrieve all", fn(table) {
    record.insert(table, "a", Admin(1))
    record.insert(table, "b", Admin(2))
    let assert [Admin(_), Admin(_)] = lamb.all(table, q.new())
  })

  setup.table("retrieve partial", fn(table) {
    record.insert(table, "a", Admin(id: 1))
    record.insert(table, "b", Admin(id: 2))
    record.insert(table, "c", Admin(id: 3))
    record.insert(table, "d", Admin(id: 4))
    record.insert(table, "e", Admin(id: 5))

    let assert Records([_, _] as a, step) = lamb.batch(table, 2, q.new())
    let assert Records([_, _] as b, step) = lamb.continue(step)
    let assert End([_] as c) = lamb.continue(step)

    let assert [Admin(_), Admin(_), Admin(_), Admin(_), Admin(_)] =
      list.concat([a, b, c])
  })

  setup.table("count", fn(table) {
    record.insert(table, "a", Admin(id: 1))
    record.insert(table, "b", Admin(id: 2))
    record.insert(table, "c", Admin(id: 3))
    record.insert(table, "d", Admin(id: 4))
    record.insert(table, "e", Admin(id: 5))

    let assert 5 = lamb.count(table, q.new())
  })
}

pub fn complex_query_test() {
  setup.table("test", fn(table) {
    record.insert(table, "a", 1)
    record.insert(table, "b", 2)
    record.insert(table, "c", 3)
    record.insert(table, "d", 4)
    record.insert(table, "e", 5)

    let assert ["c"] =
      q.new()
      |> q.bind(#(var(0), 3))
      |> q.map(var(0))
      |> lamb.all(table, _)
  })
}

pub fn debugging_test() {
  // Validate
  let assert Ok(_query) =
    q.new()
    |> q.validate()

  let assert Ok(_query) =
    q.new()
    |> q.bind(#(var(0), var(1), ignore(), atom("test")))
    |> q.map(#(atom("test"), var(1), var(0)))
    |> q.validate()

  let assert Error(_query) =
    q.new()
    |> q.map(#(var(100)))
    |> q.validate()

  // Against
  let assert Ok(_query) =
    q.new()
    |> q.against(#(1, "Value"))

  let assert Ok(_query) =
    q.new()
    |> q.bind(#(atom("user"), ignore(), "Raúl", var(0), ignore()))
    |> q.map(var(0))
    |> q.against(User(1, "Raúl", 35, ""))

  let assert Error(_query) =
    q.new()
    |> q.bind(#(atom("user"), ignore(), "Raúl", var(0), ignore()))
    |> q.map(var(0))
    |> q.against(User(1, "Carlos", 30, ""))
}

pub fn parse_tranform_query_test() {
  let table = setup.users_table(records: 33_333)
  let assert 33_333 = lamb.count(table, q.new())

  let assert [query_user] = query_user()
  let assert [query_client] = query_client()
  let assert [query_admin] = query_admin()

  let assert 11_111 = lamb.count(table, query_user)
  let assert 11_111 = lamb.count(table, query_client)
  let assert 11_111 = lamb.count(table, query_admin)

  let assert True =
    11_111
    > lamb.all(table, when_name(is: "Raúl"))
    |> list.length()

  table.delete(table)
}

@external(erlang, "artifacts_queries_ffi", "query_user")
fn query_user() -> List(q.Query(index, record))

@external(erlang, "artifacts_queries_ffi", "query_client")
fn query_client() -> List(q.Query(index, record))

@external(erlang, "artifacts_queries_ffi", "query_admin")
fn query_admin() -> List(q.Query(index, record))

fn when_name(is name: String) -> q.Query(index, record) {
  let assert [query] = query_when_name_is(name)
  query
}

@external(erlang, "artifacts_queries_ffi", "query_where_name_is")
fn query_when_name_is(name: String) -> List(q.Query(index, record))
