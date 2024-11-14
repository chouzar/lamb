// import lamb/query.{type Query}
//
// // Experimental
//
// TODO: Have an intermediate type that holds the table and bind together?
// > query
// > |> bind(fn(a, b, c) { Record("value", a, b, c) })
// > |> map(fn(x: V1(a), y: V2(b), z: V3(c)) { record(NewRecord, #(x, y, z)) })
//
// > pub type MemoizedQuery(index, record, binding, Vars3)
//
//
// pub type Shape(condition) =
//   #(#(Atom, Atom), List(condition), List(Atom))
//
// pub type Query2(condition) =
//   Shape(condition)
//
// pub fn new2() -> Query2(c) {
//   let head = #(term.var(0), term.var(1))
//   let cond = []
//   let body = [term.var(1)]
//   #(head, cond, body)
// }
//
// But make this value specific for mapping.
// query_record() ->
//     {{'_', '$1'}, [], ['$1']}.
//
// pub type Expression(x, y) {
//   Term(x)
//   Equal(Expression(x, y), Expression(x, y))
// }
// pub fn filter(
//   query: Query(index, record, head, body, x, y),
//   expression: Expression(x, y),
// ) -> Query(index, record, head, body, x, y) {
//   let conditions = [expression, ..query.conditions]
//   Function(..query, conditions: conditions)
// }
// type Condition(x) {
//   Eq(Expression, Expression)
//   Lt(Expression, Expression)
//   Gt(Expression, Expression)
//   Not(Expression)
//   // Negates its single argument (anything other than false gives false).
//   And(List(Expression))
//   // Returns true if all its arguments (variable length argument list) evaluate to true, otherwise false.
//   Or(List(Expression))
//   // Returns true if any of its arguments evaluates to true. Variable length argument list. Evaluation order is undefined.
//   Xor(Expression, Expression)
//   // Only two arguments, of which one must be true and the other false to return true; otherwise 'xor' returns false.
//   Length(List(Expression))
//   Max(Expression, Expression)
//   Min(Expression, Expression)
//   BitSize(Expression)
//   ByteSize(Expression)
//   TupleSize(Expression)
//   BinaryPart(Expression)
// }
//
//pub type Term
//
//pub type Expression(x, y) {
//  Term(x)
//  Equal(Expression(x, y), Expression(x, y))
//}
//
//pub fn filter(
//  query: Query(index, record, head, body, x, y),
//  expression: Expression(x, y),
//) -> Query(index, record, head, body, x, y) {
//  let conditions = [expression, ..query.conditions]
//  Function(..query, conditions: conditions)
//}

// TODO: Could type the shape to restrict values.
//   Then whole row and bound variables can be of the type.
//@external(erlang, "lamb_erlang_ffi", "update_body")
//fn map(query: Query(index, record), with shape: shape) -> Query(index, record)
//
// pub fn whole_row() -> Atom {
//   // The variable '$_' expands to the whole match target term.
//   atom.create_from_string("$_")
// }
//
// pub fn bound_variables() -> Atom {
//   // The variable '$$' expands to a list of the records of all bound variables in order (that is, ['$1','$2', ...]).
//   atom.create_from_string("$$")
// }
//
// TODO: When is an update really needed?
// pub fn update(
//   table: Table(index, record),
//   where query: Query(index, record),
// ) -> Result(Int, Error(index)) {
//   // TODO: Consider these scenarios for bat tables:
//   //
//   // https://www.erlang.org/doc/apps/stdlib/ets.html#update_element/4
//   //
//   // >The function fails with reason badarg in the following situations:
//   // - The table type is not set or ordered_set.
//   // - The element to update is also the key.
//
//   //
//   // https://www.erlang.org/doc/apps/stdlib/ets.html#select_replace/2
//   //
//   // > For the moment, due to performance and semantic constraints, tables of type bag
//   // are not yet supported.
//   case table {
//     TableSet(table_id) -> Ok(ffi_update(table_id, [query]))
//     TableBag(_table_id) -> {
//       // TODO: An option here is to do a select, then map through the results.
//       Error(NotSupported(Bag))
//     }
//   }
// }
//
//
// setup.table("update records", fn(table) {
//   record.insert(table, "a", user.generate_user(1))
//   record.insert(table, "b", user.generate_user(2))
//   record.insert(table, "c", user.generate_user(3))
//
//   let assert [User(_, _, _, _), User(_, _, _, _), User(_, _, _, _)] =
//     lamb.all(table, q.new())
//
//   let query =
//     q.new()
//     |> q.bind(#(v(0), #(a("user"), v(1), i(), i(), i())))
//     |> q.map(#(v(0), t2(a("admin"), v(1))))
//
//   let assert Ok(3) = lamb.update(table, query)
//   let assert [Admin(_), Admin(_), Admin(_)] = lamb.all(table, q.new())
// })
//
//
// @external(erlang, "ets", "select_replace")
// fn ffi_update(table: TableId, queries: List(Query(index, record))) -> Int
//
// TODO: For tables of type bag, instead of doing a different module we could
//       just do a runtime check before ther request. I think this could make
//       the API way more ergonomic.
// pub fn get(
//   table: Table(index, record),
//   index: index,
// ) -> Result(record, Error(index)) {
//   case table.kind {
//     Set(table_id) -> {
//       ffi_get(table_id, index) |> result.replace_error(NotFound(index))
//     }
//     Bag(_table_id) -> {
//       // TODO: Could default to limit the result to the 1st record and log a warning.
//       Error(NotSupported(Bag))
//     }
//   }
// }
//
// get(TableId, Index) ->
//     case ets:lookup(TableId, Index) of
//         [{_, Record}] ->
//             {ok, Record};
//         [] ->
//             {error, nil}
//     end.
//
// setup.table("get", fn(table) {
//   let assert Error(_) = lamb.get(table, "a")
//
//   record.insert(table, "a", user.random(1))
//   record.insert(table, "b", user.random(2))
//   record.insert(table, "c", user.random(3))
//
//   let assert Ok(a) = lamb.get(table, "a")
//   let assert Ok(_) = lamb.get(table, "b")
//   let assert Ok(_) = lamb.get(table, "c")
//   let assert Error(_) = lamb.get(table, "d")
//
//   // just make sure that `a` is actually a type/record
//   case a {
//     Admin(..) -> Nil
//     User(..) -> Nil
//     Client(..) -> Nil
//   }
// })
//
// @external(erlang, "lamb_query_erlang_ffi", "update_condition")
// pub fn filter(
//   query: Query(index, record),
//   with condition: condition,
// ) -> Query(index, record)
