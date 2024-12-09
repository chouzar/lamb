import artifacts/user.{User}
import gleam/iterator
import gleam/list
import gleeunit
import gleeunit/should
import lamb.{Bag, End, Private, Protected, Public, Records, Set}
import lamb/query
import lamb/query/term.{any, tag, var}

pub fn main() {
  gleeunit.main()
}

pub fn able_to_create_table_test() {
  let assert Ok(_) = lamb.create("test_table", Private, Set, False)
  let assert Ok(_) = lamb.create("test_table", Public, Set, False)
  let assert Ok(_) = lamb.create("test_table", Protected, Set, False)
  let assert Ok(_) = lamb.create("test_table", Private, Bag, False)
  let assert Ok(_) = lamb.create("test_table", Public, Bag, False)
  let assert Ok(_) = lamb.create("test_table", Protected, Bag, False)
}

pub fn unable_to_create_registered_tables_with_duplicate_names_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, True)
  let assert Error(_) = lamb.create("test_table", Private, Set, True)
  lamb.delete(table)
}

pub fn able_to_delete_table_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  let assert True = lamb.is_alive(table)
  lamb.delete(table)
}

pub fn able_to_retrieve_table_test() {
  let assert Ok(x) = lamb.create("test_table", Private, Set, True)
  let assert Ok(y) = lamb.from_name("test_table")
  should.be_true(x == y)
  lamb.delete(x)
  lamb.from_name("test_table") |> should.be_error()
}

pub fn unable_to_retrieve_dead_table_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, True)
  lamb.delete(table)
  lamb.from_name("test_table") |> should.be_error()
}

pub fn unable_to_retrieve_unregistered_table_test() {
  lamb.from_name("non-existant") |> should.be_error()
}

pub fn able_to_determine_table_is_alive_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  should.be_true(lamb.is_alive(table))

  lamb.delete(table)
  should.be_false(lamb.is_alive(table))
}

pub fn able_to_insert_record_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  lamb.insert(table, 1, user.generate(0))
  lamb.insert(table, 2, user.generate(1))
  lamb.insert(table, 3, user.generate(2))

  let assert 3 = lamb.count(table, query.new())
}

pub fn able_to_overwrite_record_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  lamb.insert(table, 1, user.generate(0))
  lamb.insert(table, 1, user.generate(1))
  lamb.insert(table, 1, user.generate(2))

  let assert 1 = lamb.count(table, query.new())
}

pub fn able_to_remove_records_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  lamb.insert(table, 1, user.generate(0))
  lamb.insert(table, 2, user.generate(1))
  lamb.insert(table, 3, user.generate(2))

  let assert 3 = lamb.count(table, query.new())
  lamb.remove(table, query.new())
  let assert 0 = lamb.count(table, query.new())
}

pub fn able_to_remove_specific_records_test() {
  let table = initialize_users_table(333)

  let assert 333 = lamb.count(table, query.new())

  lamb.remove(table, query.new() |> query.index(1))
  let assert 332 = lamb.count(table, query.new())

  lamb.remove(table, query.new() |> query.index(2))
  let assert 331 = lamb.count(table, query.new())

  lamb.remove(table, query.new() |> query.index(3))
  let assert 330 = lamb.count(table, query.new())

  lamb.remove(table, query.new() |> query.record(#(tag("guest"), any(), any())))
  let assert 220 = lamb.count(table, query.new())

  lamb.remove(table, query.new() |> query.record(#(tag("admin"), any())))
  let assert 110 = lamb.count(table, query.new())
}

pub fn able_to_lookup_for_specific_records_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  lamb.insert(table, 1, user.generate(0))
  lamb.insert(table, 2, user.generate(1))
  lamb.insert(table, 3, user.generate(2))

  let assert [_] = lamb.lookup(table, 1)
  let assert [_] = lamb.lookup(table, 2)
  let assert [_] = lamb.lookup(table, 3)
  let assert [] = lamb.lookup(table, 4)
}

pub fn able_to_look_if_there_are_any_records_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  lamb.insert(table, 1, user.generate(0))
  lamb.insert(table, 2, user.generate(1))
  lamb.insert(table, 3, user.generate(2))

  let assert True = lamb.any(table, 1)
  let assert True = lamb.any(table, 2)
  let assert True = lamb.any(table, 3)
  let assert False = lamb.any(table, 4)
}

pub fn able_to_search_for_specific_records_test() {
  let table = initialize_users_table(333)

  let user_spec = #(tag("user"), any(), any(), any())
  let guest_spec = #(tag("guest"), any(), any())
  let admin_spec = #(tag("admin"), any())

  let query = fn(spec: spec) { query.new() |> query.record(spec) }

  let assert 333 = lamb.search(table, query.new()) |> list.length()

  let assert 111 =
    lamb.search(table, query(user_spec))
    |> list.length()

  let assert 111 =
    lamb.search(table, query(guest_spec))
    |> list.length()

  let assert 111 =
    lamb.search(table, query(admin_spec))
    |> list.length()

  let assert 111 =
    lamb.search(table, query(admin_spec))
    |> list.length()

  let assert [_] = lamb.search(table, query.new() |> query.index(1))
  let assert [_] = lamb.search(table, query.new() |> query.index(2))
  let assert [_] = lamb.search(table, query.new() |> query.index(3))
  let assert [] = lamb.search(table, query.new() |> query.index(0))
}

pub fn able_to_map_into_custom_records_test() {
  let assert Ok(table) = lamb.create("test_table", Private, Set, False)
  let record = User(name: "Raúl", age: 35, bio: "likes his hobbies")
  lamb.insert(table, 1, record)

  let user_spec = #(tag("user"), var(1), var(2), any())
  let custom_record = fn(index, _record) { #(var(1), var(2), index) }

  let query =
    query.new()
    |> query.index(matches: var(0))
    |> query.record(matches: user_spec)
    |> query.map(into: custom_record)

  let assert [user] = lamb.search(table, query)
  let assert #("Raúl", 35, 1) = user
}

pub fn able_to_batch_for_records_test() {
  let table = initialize_users_table(333)

  let query =
    query.new() |> query.record(matches: #(tag("user"), any(), any(), any()))

  let assert Records(records, step) = lamb.batch(table, 33, query)
  let assert 33 = list.length(records)

  let assert Records(records, step) = lamb.continue(step)
  let assert 33 = list.length(records)

  let assert Records(records, step) = lamb.continue(step)
  let assert 33 = list.length(records)

  let assert End(records) = lamb.continue(step)
  let assert 12 = list.length(records)
}

pub fn able_to_count_specific_records_test() {
  let table = initialize_users_table(333)

  let user_spec = #(tag("user"), any(), any(), any())
  let guest_spec = #(tag("guest"), any(), any())
  let admin_spec = #(tag("admin"), any())

  let query = fn(spec: spec) { query.new() |> query.record(spec) }

  let assert 333 = lamb.count(table, query.new())
  let assert 111 = lamb.count(table, query(user_spec))
  let assert 111 = lamb.count(table, query(guest_spec))
  let assert 111 = lamb.count(table, query(admin_spec))
  let assert 111 = lamb.count(table, query(admin_spec))
  let assert 1 = lamb.count(table, query.new() |> query.index(1))
  let assert 1 = lamb.count(table, query.new() |> query.index(2))
  let assert 1 = lamb.count(table, query.new() |> query.index(3))
  let assert 0 = lamb.count(table, query.new() |> query.index(0))
}

fn initialize_users_table(records quantity: Int) -> lamb.Table(Int, user.User) {
  let assert Ok(table) =
    lamb.create(name: "users", access: Private, kind: Set, registered: False)

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
