import artifacts/record.{Admin}
import artifacts/setup
import gleam/list
import gleeunit
import lamb.{type MatchFunction, End, Private, Protected, Public, Records}

pub fn main() {
  gleeunit.main()
}

pub fn table_test() {
  // Able to create a private table
  let assert Ok(t0) = lamb.create_table(Private, "test_table")
  let assert True = lamb.is_alive(t0)

  // Able to create a private table with same name
  let assert Ok(t1) = lamb.create_table(Private, "test_table")
  let assert True = lamb.is_alive(t1)

  // Able to create a protected table
  let assert Ok(t2) = lamb.create_table(Protected, "test_table")
  let assert True = lamb.is_alive(t2)

  // Unable to create a public table with same name
  let assert Error(_) = lamb.create_table(Public, "test_table")

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
    let assert [] = lamb.all(table)
    lamb.insert(table, 1, record.random(1))
    let assert [_] = lamb.all(table)
    lamb.insert(table, 2, record.random(2))
    let assert [_, _] = lamb.all(table)
  })
  setup.table("delete records", fn(table) {
    let assert [] = lamb.all(table)
    lamb.insert(table, 1, record.random(1))
    let assert [_] = lamb.all(table)
    lamb.delete(table, 1)
    let assert [] = lamb.all(table)
  })
}

pub fn query_test() {
  setup.table("get", fn(table) {
    let assert Error(_) = lamb.get(table, "a")
    lamb.insert(table, "a", Admin(id: 1))
    let assert Ok(_) = lamb.get(table, "a")
  })

  setup.table("retrieve all", fn(table) {
    lamb.insert(table, "a", Admin(id: 1))
    lamb.insert(table, "b", Admin(id: 2))
    let assert [Admin(_), Admin(_)] = lamb.all(table)
  })

  setup.table("retrieve partial", fn(table) {
    lamb.insert(table, "a", Admin(id: 1))
    lamb.insert(table, "b", Admin(id: 2))
    lamb.insert(table, "c", Admin(id: 3))
    lamb.insert(table, "d", Admin(id: 4))
    lamb.insert(table, "e", Admin(id: 5))

    let assert Records([_, _] as a, step) = lamb.partial(table, by: 2)
    let assert Records([_, _] as b, step) = lamb.continue(step)
    let assert End([_] as c) = lamb.continue(step)

    let assert [Admin(_), Admin(_), Admin(_), Admin(_), Admin(_)] =
      list.concat([a, b, c])
  })

  setup.table("count", fn(table) {
    lamb.insert(table, "a", 1)
    lamb.insert(table, "b", 2)
    lamb.insert(table, "c", 3)
    lamb.insert(table, "d", 4)
    lamb.insert(table, "e", 5)

    let assert 5 = lamb.count(table)
  })
}

pub fn search_test() {
  let table = setup.users_table(records: 33_333)

  let assert 33_333 = lamb.count(table)

  let query_user = query_user()
  let query_client = query_client()
  let query_admin = query_admin()

  let assert 11_111 = lamb.search_count(table, query_user)
  let assert 11_111 = lamb.search_count(table, query_client)
  let assert 11_111 = lamb.search_count(table, query_admin)

  let query = list.concat([query_user, query_client])
  let assert 22_222 = lamb.search_count(table, query)

  let query = list.concat([query_client, query_admin])
  let assert 22_222 = lamb.search_count(table, query)

  let query = list.concat([query_user, query_client, query_admin])
  let assert 33_333 = lamb.search_count(table, query)

  let query = query_when_name_is("Raúl")
  let users = lamb.search(table, query)
  let assert True = list.length(users) < 11_111

  lamb.delete_table(table)
}

@external(erlang, "artifacts_queries_ffi", "query_user")
fn query_user() -> List(MatchFunction)

@external(erlang, "artifacts_queries_ffi", "query_client")
fn query_client() -> List(MatchFunction)

@external(erlang, "artifacts_queries_ffi", "query_admin")
fn query_admin() -> List(MatchFunction)

@external(erlang, "artifacts_queries_ffi", "query_where_name_is")
fn query_when_name_is(name: String) -> List(MatchFunction)
// @external(erlang, "erlang", "display")
// fn display(term: term) -> term
