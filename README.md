# lamb

[![Package Version](https://img.shields.io/hexpm/v/lamb)](https://hex.pm/packages/lamb)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/lamb/)

A gleam library for operating and querying ETS tables.

```gleam
import lamb.{Set, Private}
import lamb/query
import lamb/query/term as t

type User {
  User(name: String, age: Int, bio: String)
}

pub fn main() {
  // Create a table and insert 5 records.
  let assert Ok(table) = lamb.create("users", Private, Set, False)

  lamb.insert(table, 1, User("Raúl", age: 35, bio: "While at friends gatherings, plays yugioh."))
  lamb.insert(table, 2, User("César", age: 33, bio: "While outdoors, likes bird watching."))
  lamb.insert(table, 3, User("Carlos", age: 30, bio: "Always craving for coffee."))
  lamb.insert(table, 4, User("Adrián", age: 26, bio: "Simply exists."))

  // Retrieve all User records.
  let _records = lamb.search(table, query.new())

  // Retrieve all User ids.
  let to_index = fn(index, _record) { index }
  
  let query =
    query.new()
    |> query.index(t.v(0))
    |> query.map(to_index)

  let _ids = lamb.all(table, query)

  // Retrieve all User records in batches of 2.
  let assert Records([_, _], step) = lamb.batch(table, by: 2, where: q.new())
  let assert Records([_, _], step) = lamb.continue(step)
  let assert End([]) = lamb.continue(step)
}
```

The API does rely on matchspecs to query stored data, in [erlang matchspecs](https://www.erlang.org/doc/apps/erts/match_spec.html) are composed by a Tuple of arity 3 called a `MatchFunction`:

- A `Head` that contains the shape of the data we want to match to, as well as variable declarations.
- A list of `Condition` expressions that can help filter data through predicates.
- A `Body` that declares the shape and variables we'd like to output from the `MatchFunction`.

None of the current operations enforce types, querying so far is a "dynamic" operation.
