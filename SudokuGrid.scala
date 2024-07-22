import scala.collection.immutable.Set
import SudokuGrid.SudokuCell

case class SudokuGrid private (grid: Vector[Vector[SudokuCell]]):
  override def toString(): String =
    "SudokuGrid:\n" +
    grid.zipWithIndex.map((rowVec, i) => 
      val rowStr = rowVec.zipWithIndex.map((colVal, j) =>
        if j % 3 == 2 then
          s"$colVal "
        else colVal.toString()
      ).mkString(" ")

      if i % 3 == 2 then
        s"$rowStr\n"
      else rowStr.toString()
    ).mkString("\n")

  def solved(): Option[SudokuGrid] =
    ???

  infix def where(loc: ((Int, Int), Int)): SudokuGrid =
    val ((row, col), value) = loc
    this.copy(grid.updated(row, grid(row).updated(col, SudokuCell(value))))

object SudokuGrid:
  def apply(): SudokuGrid =
    new SudokuGrid(Vector.fill(9)(Vector.fill(9)(SudokuCell(0))))
  
  def apply(grid: Vector[Vector[SudokuCell]]): SudokuGrid =
    require(grid.forall(_.size == 9) && grid.size == 9)

    new SudokuGrid(grid)
  

  enum SudokuCell:
    case Known(value: Int)
    case Unknown(values: Set[Int])
    case Invalid

    infix def without(value: Int): SudokuCell = this match
      case Known(v) => this
      case Unknown(vs) =>
        val newValues = vs - value
        if newValues.isEmpty then Invalid
        else Unknown(newValues)
      case Invalid => Invalid
    
    override def toString(): String = this match
      case Known(value) => value.toString()
      case Unknown(values) => "-"
      case Invalid => "X"

  object SudokuCell:
    def apply(value: Int): SudokuCell = Known(value)
    def apply(values: Set[Int]): SudokuCell = Unknown(values)
    