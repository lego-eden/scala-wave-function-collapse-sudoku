import java.awt.Color
import javax.swing.SwingUtilities
import javax.swing.JFrame
import javax.swing.WindowConstants
import javax.swing.JPanel
import java.awt.GridLayout
import javax.swing.JTextField
import java.awt.Dimension
import javax.swing.JButton
import javax.swing.JOptionPane
import javax.swing.BoxLayout
import javax.swing.SwingConstants
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.compiletime.ops.double
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class SudokuGui(title: String, width: Int, height: Int):
  private var sudoku = SudokuGrid.empty
  private val darkColor = Color(220, 220, 220)
  private val lightColor = Color.WHITE
  SwingUtilities.invokeLater(() => createWindow(title, width, height))

  private def createWindow(title: String, width: Int, height: Int): Unit =
    val frame = JFrame(title)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    val sudokuPanel = JPanel()
    sudokuPanel.setLayout(GridLayout(9, 9))
    for 
      r <- 0 until 9
      c <- 0 until 9
    do
      sudokuPanel.add(Cell(r, c,
        if (r / 3) % 2 == 0 ^ (c / 3) % 2 == 0 then darkColor else lightColor
      ))

    sudokuPanel.setPreferredSize(Dimension(width, height))
    val solveButton = JButton("Solve")
    solveButton.addActionListener(e =>
      // Future:
      //   val original = sudoku
      //   var solution = Vector.empty[SudokuGrid]
      //   given Visualize[SudokuGrid] =
      //     s => solution = solution :+ s
      //   val solved = sudoku.solved
      //   solution.foreach(s => SwingUtilities.invokeLater(() =>
      //     sudoku = s
      //     updateCells()
      //     Thread.sleep(200)
      //   ))
      //   SwingUtilities.invokeLater(() =>
      //     solved match
      //       case None => 
      //         JOptionPane.showMessageDialog(null, "The sudoku could not be solved")
      //         sudoku = original
      //       case Some(value) => sudoku = value
      //     updateCells()
      //   )

      sudoku.solved match
        case None => JOptionPane.showMessageDialog(null, "The sudoku could not be solved")
        case Some(solution) =>
          sudoku = solution
          updateCells()
    )
    
    val clearButton = JButton("Clear")
    clearButton.addActionListener(e =>
      sudoku = SudokuGrid.empty
      updateCells()
    )

    val buttonPanel = JPanel()
    buttonPanel.add(solveButton)
    buttonPanel.add(clearButton)

    val container = JPanel()
    container.setLayout(BoxLayout(container, BoxLayout.Y_AXIS))
    container.add(sudokuPanel)
    container.add(buttonPanel)
    
    frame.add(container)
    frame.pack()
    frame.setVisible(true)

  private def updateCells(): Unit =
    Cell.cells.foreach(_.sync())

  private class Cell(row: Int, col: Int, bg: Color) extends JTextField():
    Cell.cells(row * 9 + col) = this

    setHorizontalAlignment(SwingConstants.CENTER)
    setBackground(bg)
    setFont(getFont().deriveFont(28f))
    getDocument().addDocumentListener(new DocumentListener:
      override def insertUpdate(e: DocumentEvent): Unit = changeSudoku()
      override def removeUpdate(e: DocumentEvent): Unit = sudoku = sudoku remove (row, col)
      override def changedUpdate(e: DocumentEvent): Unit = changeSudoku()

      private def changeSudoku(): Unit =
        val setSudokuGrid = Try:
          sudoku = sudoku where (row, col) -> getText().toInt
        setSudokuGrid match
          case Failure(exception) => ()
          case Success(value) => ()

        SwingUtilities.invokeLater(() => sync())
    )

    def sync(): Unit =
      setText(sudoku(row, col).replace("-", "").replace("X", ""))
  
  private object Cell:
    val cells = new Array[Cell](81)