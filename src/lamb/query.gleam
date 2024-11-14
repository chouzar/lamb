import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import gleam/int
import gleam/list

pub type Query(index, record, binding)

@external(erlang, "lamb_query_erlang_ffi", "new")
pub fn new() -> Query(index, record, binding)

pub fn bind0(
  query: Query(index, record, binding),
  with constructor: fn() -> record,
) -> Query(index, record, fn() -> record) {
  bind(query, constructor)
}

pub fn bind1(
  query: Query(index, record, binding),
  with constructor: fn(a) -> record,
) -> Query(index, record, fn(a) -> record) {
  bind(query, constructor)
}

pub fn bind2(
  query: Query(index, record, binding),
  with constructor: fn(a, b) -> record,
) -> Query(index, record, fn(a, b) -> record) {
  bind(query, constructor)
}

pub fn bind3(
  query: Query(index, record, binding),
  with constructor: fn(a, b, c) -> record,
) -> Query(index, record, fn(a, b, c) -> record) {
  bind(query, constructor)
}

pub fn bind4(
  query: Query(index, record, binding),
  with constructor: fn(a, b, c, d) -> record,
) -> Query(index, record, fn(a, b, c, d) -> record) {
  bind(query, constructor)
}

pub fn bind5(
  query: Query(index, record, binding),
  with constructor: fn(a, b, c, d, e) -> record,
) -> Query(index, record, fn(a, b, c, d, e) -> record) {
  bind(query, constructor)
}

pub fn bind6(
  query: Query(index, record, binding),
  with constructor: fn(a, b, c, d, e, f) -> record,
) -> Query(index, record, fn(a, b, c, d, e, f) -> record) {
  bind(query, constructor)
}

pub fn bind7(
  query: Query(index, record, binding),
  with constructor: fn(a, b, c, d, e, f, g) -> record,
) -> Query(index, record, fn(a, b, c, d, e, f, g) -> record) {
  bind(query, constructor)
}

pub fn bind8(
  query: Query(index, record, binding),
  with constructor: fn(a, b, c, d, e, f, g, h) -> record,
) -> Query(index, record, fn(a, b, c, d, e, f, g, h) -> record) {
  bind(query, constructor)
}

pub fn bind9(
  query: Query(index, record, binding),
  with constructor: fn(a, b, c, d, e, f, g, h, i) -> record,
) -> Query(index, record, fn(a, b, c, d, e, f, g, h, i) -> record) {
  bind(query, constructor)
}

fn bind(query, constructor) {
  let shape = ffi_build_matchhead(constructor)

  query
  |> ffi_update_head(#(var(0), shape))
  |> ffi_update_body(shape)
}

pub fn map0(
  query: Query(index, record, fn() -> record),
  with constructor: fn(index) -> x,
) -> Query(index, record, fn(index) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map1(
  query: Query(index, record, fn(a) -> record),
  with constructor: fn(index, a) -> x,
) -> Query(index, record, fn(index, a) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map2(
  query: Query(index, record, fn(a, b) -> record),
  with constructor: fn(index, a, b) -> x,
) -> Query(index, record, fn(index, a, b) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map3(
  query: Query(index, record, fn(a, b, c) -> record),
  with constructor: fn(index, a, b, c) -> x,
) -> Query(index, record, fn(index, a, b, c) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map4(
  query: Query(index, record, fn(a, b, c, d) -> record),
  with constructor: fn(index, a, b, c, d) -> x,
) -> Query(index, record, fn(index, a, b, c, d) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map5(
  query: Query(index, record, fn(a, b, c, d, e) -> record),
  with constructor: fn(index, a, b, c, d, e) -> x,
) -> Query(index, record, fn(index, a, b, c, d, e) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map6(
  query: Query(index, record, fn(a, b, c, d, e, f) -> record),
  with constructor: fn(index, a, b, c, d, e, f) -> x,
) -> Query(index, record, fn(index, a, b, c, d, e, f) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map7(
  query: Query(index, record, fn(a, b, c, d, e, f, g) -> record),
  with constructor: fn(index, a, b, c, d, e, f, g) -> x,
) -> Query(index, record, fn(index, a, b, c, d, e, f, g) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map8(
  query: Query(index, record, fn(a, b, c, d, e, f, g, h) -> record),
  with constructor: fn(index, a, b, c, d, e, f, g, h) -> x,
) -> Query(index, record, fn(index, a, b, c, d, e, f, g, h) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

pub fn map9(
  query: Query(index, record, fn(a, b, c, d, e, f, g, h, i) -> record),
  with constructor: fn(index, a, b, c, d, e, f, g, h, i) -> x,
) -> Query(index, record, fn(index, a, b, c, d, e, f, g, h, i) -> x) {
  let shape = ffi_build_matchbody(constructor)
  query |> ffi_update_body(shape)
}

type Test(body) {
  NoMatch
  Spec(body)
  Invalid(List(Charlist))
}

pub fn validate(
  query: Query(index, record, binding),
) -> Result(Query(index, record, binding), List(String)) {
  case ffi_test_query(query, #(ignore())) {
    NoMatch -> Ok(query)
    Spec(_) -> Ok(query)
    Invalid(errors) -> Error(list.map(errors, charlist.to_string))
  }
}

pub fn against(
  query: Query(index, record, binding),
  row: row,
) -> Result(shape, Nil) {
  case ffi_test_query(query, row) {
    NoMatch -> Error(Nil)
    Spec(shape) -> Ok(shape)
    Invalid(_errors) -> Error(Nil)
  }
}

fn ignore() -> Atom {
  atom.create_from_string("_")
}

pub fn var(at position: Int) -> Atom {
  case position {
    n if n >= 0 && n <= 100_000_000 ->
      atom.create_from_string("$" <> int.to_string(n))
    _other -> panic as "can only specify a variable between 0 and 100_000_000"
  }
}

@external(erlang, "lamb_query_erlang_ffi", "update_head")
fn ffi_update_head(
  query: Query(index, record, binding),
  with shape: shape,
) -> Query(index, record, binding)

@external(erlang, "lamb_query_erlang_ffi", "update_body")
fn ffi_update_body(
  query: Query(index, record, binding),
  with shape: shape,
) -> Query(index, record, x)

@external(erlang, "lamb_query_erlang_ffi", "build_matchhead")
fn ffi_build_matchhead(constructor: constructor) -> record

@external(erlang, "lamb_query_erlang_ffi", "build_matchbody")
fn ffi_build_matchbody(constructor: constructor) -> record

@external(erlang, "lamb_query_erlang_ffi", "test_query")
fn ffi_test_query(query: Query(index, record, binding), row: row) -> Test(body)
