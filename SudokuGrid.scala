import scala.collection.immutable.Set
import scala.annotation.targetName
import scala.compiletime.ops.double
import DoodleVisualizer.visualize

case class SudokuGrid private (private val grid: Vector[Vector[SudokuGrid.Cell]]):
  import SudokuGrid.{Cell, defaultCellValues}

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

  def printAffectedCells(point: (Int, Int)): Unit =
    printAffectedCells(point._1, point._2)

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
            s"${Console.REVERSED}$res${Console.RESET}"
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
    val boxColStart = (col / 3) * 3
    
    val boxRowRange = boxRowStart until (boxRowStart + 3)
    val boxColRange = boxColStart until (boxColStart + 3)
    
    val boxIndexSet = 
      boxRowRange.flatMap(row => Vector(row, row, row))
      .zip(boxColRange :++ boxColRange :++ boxColRange).toSet
    
    cellsWithIndexes.filter((cell, rowIndex, colIndex) =>
      boxIndexSet(rowIndex, colIndex)
    )

  private def affectedCells(row: Int, col: Int): Vector[(Cell, Int, Int)] =
    val nonUniqueAffectedCells = cellsWithIndexes.filter((cell, rowIndex, colIndex) =>
      rowIndex == row || colIndex == col
    ) :++ boxAt(row, col)
    nonUniqueAffectedCells.distinct

  def apply(r: Int, c: Int): String =
    require((0 until 9).contains(r) && (0 until 9).contains(c))
    grid(r)(c).toString()

  def solved(using visualize: Visualize[SudokuGrid]): Option[SudokuGrid] =
    lowestEntropy match
      case _ if !isValid => None
      case None => Some(this)
      case Some((minRow, minCol)) =>
        var isSolved = false
        grid(minRow)(minCol).possibleValues
          .map(value =>
            if isSolved then None
            else
              visualize(this)
              val updatedGrid = where((minRow, minCol) -> value)
              // updatedGrid.printAffectedCells(minRow, minCol)

              updatedGrid.solved match
                case solution@Some(_) =>
                  isSolved = true
                  solution
                case None => None
          )
          .collectFirst:
            case Some(sudoku) => sudoku

  lazy val isValid: Boolean =
    cellsWithIndexes.forall((cell, _, _) => cell.isValid)

  lazy val lowestEntropy: Option[(Int, Int)] =
    cellsWithIndexes.collect{case cell@(Cell.Unknown(_), _, _) => cell}
    .minByOption((cell, _, _) => cell.possibleValues.size)
    .map((_, row, col) => (row, col))

  private lazy val cellsWithIndexes: Vector[(Cell, Int, Int)] =
    val seq = for
      row <- 0 until 9
      col <- 0 until 9
    yield
      (grid(row)(col), row, col)
    seq.toVector

  infix def where(locIsValue: ((Int, Int), Int)): SudokuGrid =
    val ((row, col), value) = locIsValue

    if !defaultCellValues(value) then
      throw new IllegalArgumentException("Number out of range")
    
    var newGrid = updated(row, col)(grid(row)(col).into(value))

    newGrid.affectedCells(row, col).foreach((cell, rowIndex, colIndex) =>
      newGrid = newGrid.updated(rowIndex, colIndex)(cell without value)
    )

    newGrid

  infix def where(locsAndValues: ((Int, Int), Int)*): SudokuGrid =
    var newGrid = this
    locsAndValues.foreach(locIsValue =>
      newGrid = newGrid where locIsValue
    )
    newGrid

  infix def remove(loc: (Int, Int)): SudokuGrid =
    val (row, col) = loc
    val newGrid = updated(row, col)(Cell.default)
    val locsAndValues = newGrid.cellsWithIndexes
      .collect{case (Cell.Known(value), r, c) => (r, c) -> value}
    SudokuGrid.empty.where(locsAndValues*)

  private def updated(row: Int, col: Int)(value: Cell): SudokuGrid =
    SudokuGrid(grid.updated(row, grid(row).updated(col, value)))
    

object SudokuGrid:
  val defaultCellValues = Cell.defaultValues

  given Visualize[SudokuGrid] = s => ()

  val empty: SudokuGrid =
    new SudokuGrid(Vector.fill(9)(Vector.fill(9)(Cell.default)))
  
  @targetName("gridapply")
  def apply(grid: Vector[Vector[Int]]): SudokuGrid =
    require(grid.forall(_.size == 9) && grid.size == 9)
    var newGrid = SudokuGrid.empty
    for
      row <- 0 until 9
      col <- 0 until 9
    do
      if grid(row)(col) != 0 then
        newGrid = newGrid where (row, col) -> grid(row)(col)

    newGrid

  private enum Cell:
    case Known(value: Int)
    case Unknown(values: Set[Int])
    case Invalid(value: Option[Int])

    lazy val isValid: Boolean = this match
      case Invalid(value) => false
      case _ => true

    infix def into(value: Int): Cell = this match
      case Known(value) => this 
      case Unknown(values) => 
        if values(value) then Known(value) 
        else Invalid(Some(value))
      case Invalid(value) => this

    infix def without(value: Int): Cell = this match
      case Known(value) => this
      case Unknown(values) =>
        val newValues = values - value
        if newValues.isEmpty then Invalid(None)
        else Unknown(newValues)
      case Invalid(value) => this
    
    lazy val possibleValues: Set[Int] = this match
      case Known(value) => Set(value)
      case Unknown(values) => values
      case Invalid(value) => Set()
    
    // override def toString(): String = this match
    //   case Known(value) => s"${Console.GREEN}$value${Console.RESET}"
    //   case Unknown(values) => "-"
    //   case Invalid(Some(i)) => s"${Console.RED}$i${Console.RESET}"
    //   case Invalid(None) => s"${Console.RED}X${Console.RESET}"
    override def toString(): String = this match
      case Known(value) => value.toString()
      case Unknown(values) => "-"
      case Invalid(Some(value)) => value.toString()
      case Invalid(None) => "X"

  private object Cell:
    val defaultValues = Set(1 to 9 *)
    val default: Cell = Unknown(defaultValues)