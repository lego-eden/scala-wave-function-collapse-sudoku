val zs = Vector.fill(9)(0)
val impossigrid = Vector(
    zs, zs, zs, zs, zs, zs,
    Vector(1, 2, 3, 0, 0, 0, 0, 0, 0),
    Vector(4, 5, 6, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 7, 0, 0, 0, 0, 0)
  )
val impossible = SudokuGrid(impossigrid)

@main
def main(): Unit =
  println(s"This is an empty sudoku-grid:\n${SudokuGrid.empty}")
  println(s"When solved it looks like: \n${SudokuGrid.empty.solved}")

  println(s"This grid is impossible to solve:\n$impossible")
  println(s"When solved it gives back: '${impossible.solved}'")