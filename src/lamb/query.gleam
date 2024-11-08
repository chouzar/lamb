import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/int
import gleam/list

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

pub fn ignore() -> Atom {
  atom.create_from_string("_")
}

pub fn var(at position: Int) -> Atom {
  case position {
    n if n >= 0 && n <= 100_000_000 ->
      atom.create_from_string("$" <> int.to_string(n))
    _other -> panic as "can only specify a variable between 0 and 100_000_000"
  }
}

// TODO: Can these be made Head or Body specific?
pub fn atom(name: String) -> Atom {
  atom.create_from_string(name)
}

pub fn t1(a: a) {
  #(#(a))
}

pub fn t2(a: a, b: b) {
  #(#(a, b))
}

pub fn t3(a: a, b: b, c: c) {
  #(#(a, b, c))
}

pub fn t4(a: a, b: b, c: c, d: d) {
  #(#(a, b, c, d))
}

pub fn t5(a: a, b: b, c: c, d: d, e: e) {
  #(#(a, b, c, d, e))
}

pub fn record(name: String, tuple: tuple) -> x {
  let tag = atom(name)
  ffi_body_record(tag, tuple)
}

pub fn validate(
  query: Query(index, record),
) -> Result(Query(index, record), List(String)) {
  case ffi_test_query(query, #(ignore())) {
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

@external(erlang, "lamb_query_erlang_ffi", "body_record")
fn ffi_body_record(name: Atom, body: body) -> x
