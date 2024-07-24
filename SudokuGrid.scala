import scala.collection.immutable.Set
import scala.annotation.targetName
import scala.compiletime.ops.double

case class SudokuGrid private (private val grid: Vector[Vector[SudokuGrid.Cell]]):
  import SudokuGrid.Cell
  import SudokuGrid.defaultCellValues
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

  def printAffectedCells(row: Int, col: Int): Unit = 
    val affectedCells = this.affectedCells(row, col).toSet
    
    println(
      grid.zipWithIndex.map((rowVec, i) => 
        val rowStr = rowVec.zipWithIndex.map((colVal, j) =>
          val res = 
            if j % 3 == 2 then
              s"$colVal "
            else colVal.toString()
          if affectedCells(colVal, i, j) then
            s"${Console.BLACK_B}$res${Console.RESET}"
          else
            res
        ).mkString(" ")

        if i % 3 == 2 then
          s"$rowStr\n"
        else rowStr.toString()
      ).mkString("\n")
    )

  private def boxAt(row: Int, col: Int): Vector[(Cell, Int, Int)] =
    val boxRowStart = (row / 3) * 3
    val boxColStart = (row / 3) * 3
    
    val boxRowRange = boxRowStart until (boxRowStart + 3)
    val boxColRange = boxColStart until (boxColStart + 3)
    
    val boxIndexes = 
      boxRowRange.flatMap(row => Vector(row, row, row))
      .zip(boxColRange :++ boxColRange :++ boxColRange).toSet
    
    cellsWithIndexes.filter((cell, rowIndex, colIndex) =>
      boxIndexes(rowIndex, colIndex)
    )
  
  private def affectedCells(row: Int, col: Int): Vector[(Cell, Int, Int)] =
    val nonUniqueAffectedCells = cellsWithIndexes.filter((cell, rowIndex, colIndex) =>
      rowIndex == row || colIndex == col
    ) :++ boxAt(row, col)
    nonUniqueAffectedCells.distinct

  lazy val solved: Option[SudokuGrid] = None
  
  def isRowValid(row: Int): Boolean = grid(row).forall(_.isValid)
  def isColValid(col: Int): Boolean = grid.forall(_(col).isValid)
  def isBoxValid(cellRow: Int, cellCol: Int): Boolean =
    val rowStart = (cellRow / 3) * 3
    val colStart = (cellCol / 3) * 3
    var valid = true
    
    for 
      row <- rowStart until rowStart + 3
      col <- colStart until colStart + 3 
    do
      valid = valid && grid(row)(col).isValid
    
    valid
  
  lazy val isValid: Boolean =
    grid.indices.forall(i => isRowValid(i) && isColValid(i)) &&
    (0 until 3).forall(row =>
      (0 until 3).forall(col =>
        isBoxValid(row * 3, col * 3)
      )  
    )

  private lazy val cellsWithIndexes: Vector[(Cell, Int, Int)] =
    val seq = for
      row <- 0 until 9
      col <- 0 until 9
    yield
      (grid(row)(col), row, col)
    seq.toVector

  infix def where(locIsValue: ((Int, Int), Int)): SudokuGrid =
    val ((row, col), value) = locIsValue
    require(defaultCellValues(value))

    val boxVertical = ((row / 3) * 3) until ((row / 3) * 3) + 3
    val boxHorizontal = ((col / 3) * 3) until ((col / 3) * 3) + 3

    var newGrid = updated(row, col)(grid(row)(col).into(value))
    val cellsToModify = 
      newGrid.cellsWithIndexes.filter((cell, rowIndex, colIndex) => 
        rowIndex == row || colIndex == col || (boxHorizontal.contains(col) && boxVertical.contains(row))
      )

    cellsToModify.foreach((cell, rowIndex, colIndex) =>
      newGrid = newGrid.updated(rowIndex, colIndex)(cell without value)
    )

    newGrid

  infix def where(locsAndValues: ((Int, Int), Int)*): SudokuGrid =
    var newGrid = this
    locsAndValues.foreach(locIsValue =>
      newGrid = newGrid where locIsValue
    )
    newGrid

  private def updated(row: Int, col: Int)(value: Cell): SudokuGrid =
    SudokuGrid(grid.updated(row, grid(row).updated(col, value)))
    

object SudokuGrid:
  val defaultCellValues = Cell.defaultValues

  def apply(): SudokuGrid =
    new SudokuGrid(Vector.fill(9)(Vector.fill(9)(Cell.default)))
  
  @targetName("gridapply")
  def apply(grid: Vector[Vector[Int]]): SudokuGrid =
    var newGrid = SudokuGrid()
    for
      row <- 0 until 9
      col <- 0 until 9
    do
      newGrid = newGrid where (row, col) -> grid(row)(col)

    newGrid

  private enum Cell:
    case Known(value: Int)
    case Unknown(values: Set[Int])
    case Invalid

    lazy val isValid: Boolean = this match
      case Invalid => false
      case _ => true

    infix def into(value: Int): Cell = this match
      case Known(value) => this 
      case Unknown(values) => 
        if values(value) then Known(value) 
        else Invalid
      case Invalid => Invalid

    infix def without(value: Int): Cell = this match
      case Known(v) => this
      case Unknown(vs) =>
        val newValues = vs - value
        if newValues.isEmpty then Invalid
        else Unknown(newValues)
      case Invalid => Invalid
    
    lazy val possibleValues: Set[Int] = this match
      case Known(value) => Set(value)
      case Unknown(values) => values
      case Invalid => Set()
    
    override def toString(): String = this match
      case Known(value) => s"${Console.GREEN}$value${Console.RESET}"
      case Unknown(values) => "-"
      case Invalid => s"${Console.RED}X${Console.RESET}"

  private object Cell:
    val defaultValues = Set(1 to 9 *)
    val default: Cell = Unknown(defaultValues)
    