val zs = Vector.fill(9)(0)
val impossigrid = Vector(
    zs, zs, zs, zs, zs, zs,
    Vector(1, 2, 3, 0, 0, 0, 0, 0, 0),
    Vector(4, 5, 6, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 7, 0, 0, 0, 0, 0)
  )
val impossible = SudokuGrid(impossigrid)(using println)

@main
def main(): Unit =
  println(s"This is an empty sudoku-grid:\n${SudokuGrid()}")
  println(s"When solved it looks like: \n${SudokuGrid().solved}")

  println(s"This grid is impossible to solve:\n$impossible")
  println(s"When solved it gives back: '${impossible.solved}'")