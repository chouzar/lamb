import artifacts/setup
import artifacts/user.{Admin, Guest, User}
import gleam/list
import gleeunit
import lamb.{End, Records}
import lamb/query as q
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
    record.insert(table, "a", user.generate_admin(1))
    record.insert(table, "b", user.generate_guest(2))
    record.insert(table, "c", user.generate_guest(3))
    record.insert(table, "d", user.generate_user(4))
    record.insert(table, "e", user.generate_user(5))
    record.insert(table, "f", user.generate_user(6))

    let assert 6 = lamb.count(table, q.new())

    let assert 1 = lamb.count(table, q.new() |> q.bind1(Admin))
    let assert 2 = lamb.count(table, q.new() |> q.bind3(Guest))
    let assert 3 = lamb.count(table, q.new() |> q.bind4(User))
  })
}
