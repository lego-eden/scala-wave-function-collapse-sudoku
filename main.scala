@main
def main(): Unit =
  val sudoku = SudokuGrid()
  println(sudoku)

val zs = Vector.fill(9)(0)
val impossigrid = Vector(
    zs, zs, zs, zs, zs, zs,
    Vector(1, 2, 3, 0, 0, 0, 0, 0, 0),
    Vector(4, 5, 6, 0, 0, 0, 0, 0, 0),
    Vector(0, 0, 0, 7, 0, 0, 0, 0, 0)
  )
val impossible = SudokuGrid(impossigrid)(using Visualize(true))