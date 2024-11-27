import gleam/float
import gleam/int
import gleam/list

pub type User {
  User(name: String, age: Int, bio: String)
  Guest(x: Float, y: Float)
  Admin(id: Int)
}

pub fn generate(enum: Int) -> User {
  case enum {
    0 -> generate_user()
    1 -> generate_guest()
    _ -> generate_admin()
  }
}

pub fn generate_user() -> User {
  User(name: gen_name(), age: gen_age(), bio: gen_bio())
}

pub fn generate_guest() -> User {
  Guest(x: gen_location(), y: gen_location())
}

pub fn generate_admin() -> User {
  Admin(id: gen_id())
}

fn gen_id() -> Int {
  int.random(100_000)
}

fn gen_name() -> String {
  let assert [name, ..] = list.shuffle(["Raúl", "Carlos", "César", "Adrián"])
  name
}

fn gen_age() -> Int {
  int.random(100)
}

fn gen_bio() -> String {
  let a = "While at outdoors, likes to watch birds."
  let b = "While at friend gatherings, plays yugioh."
  let c = "While at internet, craves for new books."

  let assert [bio, ..] = list.shuffle([a, b, c])
  bio
}

fn gen_location() -> Float {
  float.random()
}
