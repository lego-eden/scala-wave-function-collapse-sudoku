//> using test.dep org.scalameta::munit::1.0.0

class SudokuTests extends munit.FunSuite:

  test("EmptySolve"):
    assert(SudokuGrid().solved().nonEmpty)
  
  test("ImpossibleSolve"):
    val unsolvableSudoku = SudokuGrid() where ((0, 0) -> 1)
    assert(SudokuGrid().solved().isEmpty)
    

end SudokuTests