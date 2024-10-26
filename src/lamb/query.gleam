import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/int
import gleam/list
import gleam/string

pub type Query(index, record)

@external(erlang, "lamb_query_erlang_ffi", "new")
pub fn new() -> Query(index, record)

@external(erlang, "lamb_query_erlang_ffi", "update_head")
pub fn bind(
  query: Query(index, record),
  with shape: shape,
) -> Query(index, record)

@external(erlang, "lamb_query_erlang_ffi", "update_condition")
pub fn filter(
  query: Query(index, record),
  with condition: condition,
) -> Query(index, record)

// TODO: Could type the shape to restrict values.
//   Then whole row and bound variables can be of the type.
@external(erlang, "lamb_query_erlang_ffi", "update_body")
pub fn map(
  query: Query(index, record),
  with shape: shape,
) -> Query(index, record)

pub fn i() -> Atom {
  atom.create_from_string("_")
}

pub fn v(at position: Int) -> Atom {
  case position {
    n if n >= 0 && n <= 100_000_000 ->
      atom.create_from_string("$" <> int.to_string(n))
    _other -> panic as "can only specify a variable between 0 and 100_000_000"
  }
}

pub fn a(name: String) -> Atom {
  let name = string.lowercase(name)
  atom.create_from_string(name)
}

pub fn validate(
  query: Query(index, record),
) -> Result(Query(index, record), List(String)) {
  case ffi_test_query(query, #(i())) {
    NoMatch -> Ok(query)
    Spec(_) -> Ok(query)
    Invalid(errors) -> Error(list.map(errors, charlist.to_string))
  }
}

pub fn against(query: Query(index, record), row: row) -> Result(shape, Nil) {
  case ffi_test_query(query, row) {
    NoMatch -> Error(Nil)
    Spec(shape) -> Ok(shape)
    Invalid(_errors) -> Error(Nil)
  }
}

type Test(body) {
  NoMatch
  Spec(body)
  Invalid(List(Charlist))
}

@external(erlang, "lamb_query_erlang_ffi", "test_query")
fn ffi_test_query(query: Query(index, record), row: row) -> Test(body)
