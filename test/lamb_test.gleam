import artifacts/record.{Admin, User, Client}
import artifacts/setup
import gleam/list
import gleeunit
import lamb.{End, Private, Protected, Public, Records, Set}
import lamb/query.{a, i, t2, v} as q

pub fn main() {
  gleeunit.main()
}

pub fn table_test() {
  // Able to create a private table
  let assert Ok(t0) = lamb.create_table(Set, Private, "test_table")
  let assert True = lamb.is_alive(t0)

  // Able to create a private table with same name
  let assert Ok(t1) = lamb.create_table(Set, Private, "test_table")
  let assert True = lamb.is_alive(t1)

  // Able to create a protected table
  let assert Ok(t2) = lamb.create_table(Set, Protected, "test_table")
  let assert True = lamb.is_alive(t2)

  // Unable to create a public table with same name
  let assert Error(_) = lamb.create_table(Set, Public, "test_table")

  // Able to retrieve a table by name
  let assert Ok(_) = lamb.from_name("test_table")

  // Able to delete a table
  lamb.delete_table(t0)
  let assert False = lamb.is_alive(t0)

  lamb.delete_table(t1)
  let assert False = lamb.is_alive(t1)

  lamb.delete_table(t2)
  let assert False = lamb.is_alive(t2)
  let assert Error(_) = lamb.from_name("test_table")
}

pub fn record_test() {
  setup.table("insert records", fn(table) {
    let assert [] = lamb.all(table, q.new())

    lamb.insert(table, [#("a", record.random(1))])
    let assert [_] = lamb.all(table, q.new())

    lamb.insert(table, [#("b", record.random(2))])
    let assert [_, _] = lamb.all(table, q.new())

    lamb.insert(table, [#("c", record.random(3))])
    let assert [_, _, _] = lamb.all(table, q.new())
  })

  setup.table("update records", fn(table) {
    lamb.insert(table, [
      #("a", record.generate_user(1)),
      #("b", record.generate_user(2)),
      #("c", record.generate_user(3)),
    ])

    let assert [User(_, _, _, _), User(_, _, _, _), User(_, _, _, _)] =
      lamb.all(table, q.new())

    let query =
      q.new()
      |> q.bind(#(v(0), #(a("user"), v(1), i(), i(), i())))
      |> q.map(#(v(0), t2(a("admin"), v(1))))

    let assert 3 = lamb.update(table, query)
    let assert [Admin(_), Admin(_), Admin(_)] = lamb.all(table, q.new())
  })

  setup.table("delete records", fn(table) {
    let assert [] = lamb.all(table, q.new())

    lamb.insert(table, [
      #("a", record.random(1)),
      #("b", record.random(2)),
      #("c", record.random(3)),
    ])

    let assert 3 = lamb.delete(table, q.new())
    let assert [] = lamb.all(table, q.new())
  })
}

pub fn simple_query_test() {
  setup.table("get", fn(table) {
    let assert Error(_) = lamb.get(table, "a")

    lamb.insert(table, [
      #("a", record.random(1)),
      #("b", record.random(2)),
      #("c", record.random(3)),
    ])

    let assert Ok(a) = lamb.get(table, "a")
    let assert Ok(_) = lamb.get(table, "b")
    let assert Ok(_) = lamb.get(table, "c")
    let assert Error(_) = lamb.get(table, "d")

    // just make sure that `a` is actually a type/record
    case a {
      Admin(..) -> Nil
      User(..) -> Nil
      Client(..) -> Nil
    }
  })

  setup.table("retrieve all", fn(table) {
    lamb.insert(table, [#("a", Admin(1)), #("b", Admin(2))])
    let assert [Admin(_), Admin(_)] = lamb.all(table, q.new())
  })

  setup.table("retrieve partial", fn(table) {
    lamb.insert(table, [
      #("a", Admin(id: 1)),
      #("b", Admin(id: 2)),
      #("c", Admin(id: 3)),
      #("d", Admin(id: 4)),
      #("e", Admin(id: 5)),
    ])

    let assert Records([_, _] as a, step) = lamb.batch(table, 2, q.new())
    let assert Records([_, _] as b, step) = lamb.continue(step)
    let assert End([_] as c) = lamb.continue(step)

    let assert [Admin(_), Admin(_), Admin(_), Admin(_), Admin(_)] =
      list.concat([a, b, c])
  })

  setup.table("count", fn(table) {
    lamb.insert(table, [
      #("a", Admin(id: 1)),
      #("b", Admin(id: 2)),
      #("c", Admin(id: 3)),
      #("d", Admin(id: 4)),
      #("e", Admin(id: 5)),
    ])

    let assert 5 = lamb.count(table, q.new())
  })
}

pub fn complex_query_test() {
  setup.table("test", fn(table) {
    lamb.insert(table, [#("a", 1), #("b", 2), #("c", 3), #("d", 4), #("e", 5)])

    let assert ["c"] =
      q.new()
      |> q.bind(#(v(0), 3))
      |> q.map(v(0))
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
    |> q.bind(#(v(0), v(1), i(), a("test")))
    |> q.map(#(a("test"), v(1), v(0)))
    |> q.validate()

  let assert Error(_query) =
    q.new()
    |> q.map(#(v(100)))
    |> q.validate()

  // Against
  let assert Ok(_query) =
    q.new()
    |> q.against(#(1, "Value"))

  let assert Ok(_query) =
    q.new()
    |> q.bind(#(a("user"), i(), "Raúl", v(0), i()))
    |> q.map(v(0))
    |> q.against(User(1, "Raúl", 35, ""))

  let assert Error(_query) =
    q.new()
    |> q.bind(#(a("user"), i(), "Raúl", v(0), i()))
    |> q.map(v(0))
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

  lamb.delete_table(table)
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
