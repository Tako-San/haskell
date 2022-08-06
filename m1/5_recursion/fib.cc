#include <iostream>

int fibRec(int n)
{
  if (n == 0)
    return 0;

  if (n == 1)
    return 1;

  if (n > 0)
    return fibRec(n - 2) + fibRec(n - 1);

  return fibRec(n + 2) - fibRec(n + 1);
}

int fib(int n)
{
  if (n == 0)
    return 0;

  if (n == 1)
    return 1;

  int t1 = 0;
  int t2 = 1;
  int next = t1 + t2;

  for (int i = 2; i < n; ++i)
  {
    t1 = t2;
    t2 = next;
    next = t1 + t2;
  }

  return next;
}

int main()
{
  int n = 0;
  std::cin >> n;
  std::cout << fibRec(n) << std::endl;
  std::cout << fib(n) << std::endl;
  return 0;
}