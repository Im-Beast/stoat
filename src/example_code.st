/* import builtins::{
  print
};

import ./example_code.st::{
  dog,
  nested_module::{
    cat
  }
};

export fun dog(i: i32) -> i32 {
  return i * 2;
}

export mod nested_module {
  export fun cat(i: i32) -> i32 {
    return i * 3;
  }
} */

let char = ' ';

let mut unknown_i = 3;
let unknown_f = 3.14159;

let implicit_bool = false;
let explicit_bool: bool = true;

let explicit_i32: i32 = 5;
let implici_i32 = 5i32;

let implicit_f32 = 5f32;
let implicit_f64 = 32f64;
let explicit_f32: f32 = 5f32;
let explicit_f64: f64 = 32f64;

let implicit_str = "dog";
let explicit_str: str = "dog";

let implicit_char = 'c';
let explicit_char: char = 'c';

let implicit_tuple = (1i32, 2i32, 3i32);
let implicit_tuple = (1+2, 3i32, "dog");
let explicit_tuple: (i32, i32, i32) = (1, 2, 3);

let implicit_arr = [1i32, 2i32, 3i32];
let implicit_arr = [1+2, 2, 5i32];
let explicit_arr: i32[3] = [1, 2, 3];

let implicit_vec = [|1i32, 2i32|];
let explicit_vec: i32[] = [|1, 2|];

let cat = "cat";

let implicit_ref = &cat;
let explicit_ref: &str = &cat;

let implicit_mut_ref = &mut cat;
let explicit_mut_ref: &mut str = &mut cat;

let negative_i32: i32 = -5;
let test = !false;

"dog";

c + a;

if a > b {
  print("hello {}", "world", 'x', 1, 1.2);
}

if a != b {}
if a == b {}
if a > b {}
if a < b {}

fun add(a: i32, b: i32) -> i32 {
  a + b
}

fun mul(a: i32, b: i32) -> i32 {
  if a > b {
    return a * b;
  }

  return a * b;
}


// this crashes
if !("ahu" != 2) {
  print("yep");
}

add(1, 2);

{
  1;
  2;
  1
}

struct Dog {
  woof: bool
}

let dog = Dog { woof: false };

dog.woof = true;

enum Animal {
  Dog(AnimalID, str),
  Cat(AnimalID, str)
}

enum AnimalID {
  Dog = 0,
  Cat = 1
}

let pupper = Animal::Dog(AnimalID::Dog, "woofer junior");
let kitty = Animal::Cat(AnimalID::Cat, "meowser");

let x = if true {
  dog();
} else {
  lol(); 
};

for i in 0..10 {
  print("{}", i);
}

for j in 0..=9 {
  print("{}", j);
}

let mut i = 0;
loop {
  if i >= 10 {
    break;
  }
  print("hello");
  i += 1;
}

let mut i = 0;
while i < 10 {
  print("{}", i);
  i += 1;
}

THE_END;