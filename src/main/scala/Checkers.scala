object Checkers extends App {
  val board = new Board
  //board.king(5, 4)
  //println(board.value(5, 4))
  /*board.tab(2)(1) = board.empty
  board.tab(5)(4) = board.empty
  board.tab(3)(2) = board.white
  board.tab(4)(3) = board.black*/
  //board.move(1, 0, 2, 1)
  //println(board.value(3, 2))
  //board.move(5, 4, 4, 3)
  //println(board.value(5, 4))
  //println(board.value(3, 2))
  //board.move(5, 4, 2, 1)
  /*def getInput(): Int = {
      board.drawBoard()
      println(board.objectiveFunction())
      println("Make a move (Correct example: 3 2 4 3)")
      val in1 = scala.io.StdIn.readInt()
      val in2 = scala.io.StdIn.readInt()
      val in3 = scala.io.StdIn.readInt()
      val in4 = scala.io.StdIn.readInt()
      board.move(in1,in2,in3,in4)
      getInput()
  }
  getInput()*/
  while (true) {
    board.drawBoard()
    println(board.objectiveFunction())
    println(board.canStrike(4, 3))
    print("Czarne bij")
    println(board.strikers("black"))
    println(board.strikers("white"))
    println("Make a move (Correct example: 3 2 4 3)")
    val in1 = scala.io.StdIn.readInt()
    val in2 = scala.io.StdIn.readInt()
    val in3 = scala.io.StdIn.readInt()
    val in4 = scala.io.StdIn.readInt()
    if (!board.move(in1, in2, in3, in4))
      println("Error")
  }
}
