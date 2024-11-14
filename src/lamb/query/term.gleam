// import gleam/erlang/atom.{type Atom}
// import gleam/int
// 
// // TODO: Can these be made Head, Conditional or Body specific?
// // TODO: Maybe make recursive record query for the body?
// 
// // --- Bind specific --- //
// pub fn ignore() -> Atom {
//   atom.create_from_string("_")
// }
// 
// // --- Map specific --- //
// // Posiblemente mejor que un algebra...un mapeo de record.
// // Si yo se que mi binding is un constructor de 3:
// // * Mapping en compilación de un constructor de 3 ó
// // * Cualquier mapeo de un constructor de 0 a 3 validado en runtime ó
// // * fn(v0, v1, v2, v3) -> new_record
// pub type BodyTerm(x) {
//   Index
//   Record
//   Var(Int)
//   Val(x)
// }
// 
// // query.new()
// // |> bind(User)
// // |> map(fn(id, _, _) { #(index(), Admin(id)) })
// // |> map(index())
// 
// pub type Body(x, y, z) {
//   T1(BodyTerm(x))
//   T2(BodyTerm(x), BodyTerm(y))
//   T3(BodyTerm(x), BodyTerm(y), BodyTerm(z))
// }
// 
// pub fn record() -> Atom {
//   // These may need refinement from an Intermediate query type.
//   atom.create_from_string("$_")
// }
// 
// pub fn index() -> Atom {
//   var(0)
// }
// 
// // --- Bind, Map or Conditional --- //
// pub fn var(at position: Int) -> Atom {
//   case position {
//     n if n >= 0 && n <= 100_000_000 ->
//       atom.create_from_string("$" <> int.to_string(n))
//     _other -> panic as "can only specify a variable between 0 and 100_000_000"
//   }
// }
// 
