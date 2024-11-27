import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/list
import lamb/query/term as t

pub type Query(index, record, x) =
  #(Head(index, record), Conditions, Body(x))

type Head(index, record) =
  #(index, record)

type Conditions =
  List(Nil)

type Body(x) =
  List(x)

pub fn new() -> Query(Atom, Atom, Atom) {
  let head = #(t.any(), t.any())
  let body = t.records()
  #(head, [], [body])
}

pub fn index(query: Query(a, b, c), matches spec: spec) -> Query(spec, b, c) {
  let #(#(_index, record), conditions, body) = query
  let head = #(spec, record)
  #(head, conditions, body)
}

pub fn record(query: Query(a, b, c), matches spec: spec) -> Query(a, spec, c) {
  let #(#(index, _record), conditions, body) = query
  let head = #(index, spec)
  #(head, conditions, body)
}

pub fn map(
  query: Query(a, b, c),
  into mapper: fn(a, b) -> spec,
) -> Query(a, b, spec) {
  let #(head, conditions, _body) = query
  let #(index, record) = head
  let spec = mapper(index, record) |> ffi_wrap_tuple()
  #(head, conditions, [spec])
}

@external(erlang, "lamb_erlang_ffi", "wrap_tuple")
fn ffi_wrap_tuple(spec: spec) -> x

// --- Test API --- //

type Test(body) {
  NoMatch
  Spec(body)
  Invalid(List(Charlist))
}

pub fn validate(
  query: Query(index, record, binding),
) -> Result(Query(index, record, binding), List(String)) {
  case ffi_test_query(query, #(t.any())) {
    NoMatch -> Ok(query)
    Spec(_) -> Ok(query)
    Invalid(errors) -> Error(list.map(errors, charlist.to_string))
  }
}

@external(erlang, "lamb_erlang_ffi", "test_query")
fn ffi_test_query(query: Query(index, record, binding), row: row) -> Test(body)
