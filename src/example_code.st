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
let float = 3.14159;
c += 2;

if a > b {
  print("hello {}", "world");
}

if a != b {}
if a == b {}
if a > b {}
if a < b {}

THE_END;