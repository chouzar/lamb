import gleam/float
import gleam/int
import gleam/list

pub type User {
  User(id: Int, name: String, age: Int, bio: String)
  Guest(id: Int, x: Float, y: Float)
  Admin(id: Int)
}

pub fn random(id: Int) -> User {
  let assert [record, ..] = list.shuffle([0, 1, 2])
  generate(record, id)
}

pub fn generate(enum: Int, id: Int) -> User {
  case enum {
    0 -> generate_user(id)
    1 -> generate_guest(id)
    _ -> generate_admin(id)
  }
}

pub fn generate_user(id: Int) -> User {
  User(id: id, name: gen_name(), age: gen_age(), bio: gen_bio())
}

pub fn generate_guest(id: Int) -> User {
  Guest(id: id, x: gen_location(), y: gen_location())
}

pub fn generate_admin(id: Int) -> User {
  Admin(id: id)
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
