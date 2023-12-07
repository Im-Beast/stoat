def fibonacciLoop(n):
  num = 0;
  num2 = 1;
  temp = 0;

  for i in range(n):
    temp = num;
    num = num2;
    num2 += temp;

  return num2;

print(fibonacciLoop(91));
