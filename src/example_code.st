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

let a: i32 = 5;
let b = 5i32;
let mut c = 3;
let d = 3.14159;
let e = false;

c + a;

if a > b {
  print("hello {}", "world", 'x', 1, 1.2);
}

if a != b {}
if a == b {}
if a > b {}
if a < b {}

/*fun add(a: i32, b: i32) -> i32 {
  a + b
}*/

add(1, 2);

/*
struct Dog {
  woof: bool
}

let dog = Dog { woof: false }
*/

dog.woof = true;

THE_END;