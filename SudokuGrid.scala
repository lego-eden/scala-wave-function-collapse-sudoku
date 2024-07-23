import scala.collection.immutable.Set
import scala.annotation.targetName

case class SudokuGrid private (private val grid: Vector[Vector[SudokuGrid.SudokuCell]]):
  import SudokuGrid.SudokuCell

  require(grid.forall(_.size == 9) && grid.size == 9)

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

  def solved: Option[SudokuGrid] =
    None

  infix def where(loc: ((Int, Int), Int)): SudokuGrid =
    val ((row, col), value) = loc
    this.copy(grid.updated(row, grid(row).updated(col, SudokuCell.Known(value))))

  infix def where(locs: ((Int, Int), Int)*): SudokuGrid =
    var SudokuGrid(newGrid) = this
    locs.foreach(loc =>
      val ((row, col), value) = loc
      newGrid = newGrid.updated(row, newGrid(row).updated(col, SudokuCell.Known(value)))
    )
    SudokuGrid(newGrid)

object SudokuGrid:
  def apply(): SudokuGrid =
    new SudokuGrid(Vector.fill(9)(Vector.fill(9)(SudokuCell.Known(0))))
  
  @targetName("gridapply")
  def apply(grid: Vector[Vector[Int]]): SudokuGrid =
    new SudokuGrid(grid.map(_.map(SudokuCell.Known(_))))

  private enum SudokuCell:
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
  
  private object SudokuCell:
    def default(): SudokuCell = Unknown(Set(1 to 9 *))
    