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
  SudokuGui("SudokuGui", 800, 800)