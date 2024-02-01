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

let explicit_i32: i32 = 5;
let implici_i32 = 5i32;
let mut unknown_i = 3;
let unknown_f = 3.14159;
let implicit_bool = false;
let explicit_f32: f32 = 5f32;
let explicit_f64: f64 = 32f64;

let negative_i32: i32 = -5;
let test = !false;

/*
let implicit_tuple = (1i32, 2i32, 3i32);
let explicit_tuple: (i32, i32, i32) = (1, 2, 3);

let implicit_arr = [1i32, 2i32, 3i32];
let explicit_arr: i32[3] = [1, 2, 3];

let implicit_vec = [|1i32, 2i32|];
let explicit_vec: i32[] = [|1, 2|];
*/

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
if !(1 != 2) {
  print("yep");
}

add(1, 2);

{
  1;
  2;
  1
}

/*
struct Dog {
  woof: bool
}

let dog = Dog { woof: false }
*/

dog.woof = true;

let x = if true {
  dog();
} else {
  lol(); 
};

THE_END;