//> using test.dep org.scalameta::munit::1.0.0

class SudokuTests extends munit.FunSuite:

  test("EmptySolve"):
    assert(SudokuGrid.empty.solved.nonEmpty)
  
  test("ImpossibleSolve"):
    val unsolvableSudoku = SudokuGrid.empty where ((0, 0) -> 1)
    assert(SudokuGrid.empty.solved.isEmpty)
end SudokuTests