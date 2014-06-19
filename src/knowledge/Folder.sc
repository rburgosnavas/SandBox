type ??? = Nothing

val nums = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

nums.foldLeft(0)((x, y) => (y * y) + x)

def sumOfSquares(xs: List[Int]): Int = {
  def loop(acc: Int, list: List[Int]): Int = {
    if (list.isEmpty) acc
    else loop((list.head * list.head) + acc, list.tail)
  }
  loop(0, xs)
}

sumOfSquares(nums)

// My weird version of a "fold" like operation
def folder(init: Int)(f: (Int, Int) => Int, xs: List[Int]): Int = {
  def loop(acc: Int, f: (Int, Int) => Int, list: List[Int]): Int = {
    if (list.isEmpty) acc
    else loop(f(list.head, init) + acc, f, list.tail)
  }
  loop(0, f, xs)
}

// Using the weird folder to generalize sumOfSquares
folder(0)((x, y) => (x * x) + y, nums)
folder(0)(_ + _, nums)


