case class SudokuGrid private (grid: Vector[Vector[Int]]):
  override def toString(): String =

    "SudokuGrid:\n" +
    grid.zipWithIndex.map((rowVec, i) => 
      val rowStr = rowVec.zipWithIndex.map((colVal, j) =>
        if j % 3 == 2 then
          colVal + " "
        else colVal.toString()
      ).mkString(" ")

      if i % 3 == 2 then
        rowStr + "\n"
      else rowStr.toString()
    ).mkString("\n")

object SudokuGrid:
  def apply(): SudokuGrid =
    new SudokuGrid(Vector.fill(9)(Vector.fill(9)(0)))
  
  def apply(grid: Vector[Vector[Int]]): SudokuGrid =
    require(grid.forall(_.size == 9) && grid.size == 9)

    new SudokuGrid(grid)