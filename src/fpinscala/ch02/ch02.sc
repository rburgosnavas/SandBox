// Ex. 3
def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a, b)

// Ex. 4
def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

// Ex. 5
def compose[A, B, C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))
