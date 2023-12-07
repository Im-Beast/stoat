function fibonacciLoop(n) {
  let num = 0;
  let num2 = 1;
  let temp = 0;

  let i = 0;
  while (i < n) {
    temp = num;
    num = num2;
    num2 += temp;
    i += 1;
  }

  return num2;
}

console.log(fibonacciLoop(30));
