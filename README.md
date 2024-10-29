# lamb

[![Package Version](https://img.shields.io/hexpm/v/lamb)](https://hex.pm/packages/lamb)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/lamb/)

A gleam library for operating and querying ETS tables.

```gleam
import gleam/list
import lamb.{Set, Private}
import lamb/query.{v, i, a} as q

pub fn main() {
  // Create a table and insert 5 records.
  let assert Ok(table) = lamb.create_table(Set, Private, "test_table")

  lamb.insert(table, [
    #("a",  1),
    #("b",  2),
    #("c",  3),
    #("d",  4),
    #("e",  5),
  ])

  // Retrieve just 1 record.
  let assert Ok(3) = lamb.get(table, "c")

  // This query syntax builds a matchspec to make queries on the table.
  let query =
    q.new()
    |> q.bind(#(v(0), v(1)))

  // Retrieve all rows but only return the record
  let _records = lamb.all(table, query |> q.map(v(1)))

  // Retrieve all rows but only return the index
  let _records = lamb.all(table, query |> q.map(v(0)))

  // Retrieve all records in batches of 2.
  let assert Records([_, _] as a, step) = lamb.partial(table, by: 2, where: q.new())
  let assert Records([_, _] as b, step) = lamb.continue(step)
  let assert End([_] as c) = lamb.continue(step)
}
```

Further documentation can be found at <https://hexdocs.pm/lamb>.

## Development

```sh
gleam run   # Run the project
gleam test  # Run the tests
```

---

Work in progress notes.

# Table API

Currently viewing the table creating in terms of protection levels:
  - `Private`, private table, not named.
  - `Protected`, protected table, named.
  - `Public`, public table, named.

If we would like more precision than this, maybe having an `options` helper with default options would
help to tinker with tables:

```gleam
pub fn options() -> Options {
  https://www.erlang.org/doc/apps/stdlib/ets.html#info/1
  todo
}
```

Still need to figure out what is going to be the API for differentiating between `set` and `bag` tables.

# Query API

Matchspecs are composed by a Tuple of arity 3 called a `MatchFunction`:

- A `Head` that contains the shape of the data we want to match to, as well as variable declarations.
- A list of `Condition` expressions that can help filter data through predicates.
- A `Body` that declares the shape and variables we'd like to output from the `MatchFunction`.

```erlang
{Head, [Condition], [Body]}
```

Operating on these 3 pieces may be tackled in different ways.

## Alternative 1

Have an API that composes matchspecs together with a builder pattern:

```gleam
let query =
  q.new()                                  // Builds a basic default match function.
  |> q.bind(Person)                        // Modifies the head to match the constructor.
  |> q.match(field: 5, "Citizen")          // Modifies the head to match the value.
  |> q.where(field: 3, op: ">=", than: 18) // Adds a condition expression.

query |> q.select(object())                // Returns the whole object #(index, record).
query |> q.select(index())                 // Returns just the index of record.
query |> q.select(#(v(2), v(1)))           // Returns the 2nd and 1st variables in a tuple.
query |> q.select(False)                   // Returns false for each matching record.
query |> q.select(Driver)                  // Maps the variables to a constructor.
query |> q.select(fn(last, first) {        // Alternative mapper to above.
  Driver(last, first)
})
```

## Alternative 2

Build a "nice" parser to compose complex queries:

```gleam

let query = "
  from Person
  select 2, 1
  where 'Citizen' = 5
  where 3 >= 18
  select #(0, 1, 2)
"
```

## Alternative 3

Just build a more straightforward mapper for matchspecs.

```gleam
let head = #(any(), record("Person", v(1), v(2), v(3), "Citizen"))
let condition = #(">=", v(3), 18)
let body = body(record("Driver", v(2), v(1)))
let query_driver_elegibility = #(head, [condition], [body])
lamb.search(table, [query_driver_elegibility])
```

## Notes on types and validation

None of these alternatives currently provide a good way of enforcing types but are meant to fail gracefully if
there are errors. Querying so far is a "dynamic" operation.

An alternative would be to run validations at runtime with the help of a schema but that would
be quite a big lift for my current purposes.

Maybe another way is to provide a validator at init time, but this would exclusively check the validity of the
matchspecs.

# Checkout these ETS APIs

* https://www.erlang.org/doc/apps/stdlib/ets.html#select_delete/2
* https://www.erlang.org/doc/apps/stdlib/ets.html#fun2ms/1
* https://www.erlang.org/doc/apps/stdlib/ets.html#test_ms/2
* https://www.erlang.org/doc/apps/stdlib/ets.html#table/2
* https://www.erlang.org/doc/apps/stdlib/qlc
* https://www.erlang.org/doc/apps/stdlib/erl_parse.html#abstract/1
