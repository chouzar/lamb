import artifacts/setup
import artifacts/user.{Admin, User}
import gleeunit
import lamb
import lamb/query as q

pub fn main() {
  gleeunit.main()
}

pub fn complex_query_test() {
  setup.table("test", fn(table) {
    lamb.insert(table, "a", Admin(id: 1))
    lamb.insert(table, "b", Admin(id: 2))
    lamb.insert(table, "c", Admin(id: 3))
    lamb.insert(table, "d", Admin(id: 4))
    lamb.insert(table, "e", Admin(id: 5))

    let assert ["c"] =
      q.new()
      |> q.bind0(fn() { Admin(3) })
      |> q.map0(fn(index) { index })
      |> lamb.all(table, _)
  })

  setup.table("test", fn(table) {
    lamb.insert(table, "a", user.generate_admin(1))
    lamb.insert(table, "b", user.generate_guest(2))
    lamb.insert(table, "c", user.generate_guest(3))
    lamb.insert(table, "d", user.generate_user(4))
    lamb.insert(table, "e", user.generate_user(5))
    lamb.insert(table, "f", user.generate_user(6))

    q.new()
    |> q.bind4(User)
    |> q.map4(fn(index, id, _name, _age, _bio) { #(index, Admin(id)) })
    |> lamb.all(table, _)
  })
}
// pub fn debugging_test() {
//   // Default query should pass
//   let assert Ok(_query) =
//     q.new()
//     |> q.validate()
//
//   // Should pass when requesting index and var '$1'
//   let assert Ok(_query) =
//     q.new()
//     |> q.bind1(Admin)
//     |> q.map(#("data", t.var(1), t.var(0)))
//     |> q.validate()
//
//   // Errors when non-bound variable is mapped
//   let assert Error(_query) =
//     q.new()
//     |> q.map(#(t.var(100)))
//     |> q.validate()
//
//   // Against
//   let assert Ok(_query) =
//     q.new()
//     |> q.against(#(1, "Value"))
//
//   let assert Ok(_query) =
//     q.new()
//     |> q.bind(#(t.atom("user"), t.ignore(), "Raúl", t.var(0), t.ignore()))
//     |> q.map(t.var(0))
//     |> q.against(User(1, "Raúl", 35, ""))
//
//   let assert Error(_query) =
//     q.new()
//     |> q.bind(#(t.atom("user"), t.ignore(), "Raúl", t.var(0), t.ignore()))
//     |> q.map(t.var(0))
//     |> q.against(User(1, "Carlos", 30, ""))
// }
//