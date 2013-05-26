def fibs():
  a, b = 0, 1
  while a < 4000000:
    if (a % 2 == 0):
      yield a
    a, b = b, a+b

def answer():
  return sum(fibs())
