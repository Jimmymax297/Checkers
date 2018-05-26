object Checkers extends App {
  val white: Int = -1
  val empty: Int = 0
  val black: Int = 1
  val whiteKing: Int = -2
  val blackKing: Int = 2
//  val t = Array(
//    Array(empty, black, empty, black, empty, black, empty, black),
//    Array(black, empty, black, empty, black, empty, black, empty),
//    Array(empty, black, empty, black, empty, black, empty, black),
//    Array(empty, empty, empty, empty, empty, empty, empty, empty),
//    Array(empty, empty, empty, empty, empty, empty, empty, empty),
//    Array(white, empty, white, empty, white, empty, white, empty),
//    Array(empty, white, empty, white, empty, white, empty, white),
//    Array(white, empty, white, empty, white, empty, white, empty))
  val t = Array(
  Array(empty, black, empty, empty, empty, empty, empty, empty),
  Array(empty, empty, white, empty, empty, empty, empty, empty),
  Array(empty, empty, empty, empty, empty, empty, empty, empty),
  Array(empty, empty, empty, empty, white, empty, white, empty),
  Array(empty, empty, empty, empty, empty, empty, empty, empty),
  Array(empty, empty, white, empty, white, empty, empty, empty),
  Array(empty, empty, empty, empty, empty, empty, empty, empty),
  Array(empty, empty, empty, empty, empty, empty, empty, empty)
  )
  val board = new Board(t)
//  board.move(2, 1, 3, 2)
//  board.move(5, 4, 4, 3)
//  board.move(5,6,4,5)
  //board.move()
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
    //println(board.canStrike(4, 3))
    /*print("Czarne bijace: ")
    println(board.strikers("black"))
    print("Biale broniace: ")
    println(board.strikers("white"))*/
    println("BEGIN")
    val l = board.findStrikePath(0, 1, new Movement(List[(Int, Int, Int, Int)]()), board)
    for {
      x <- 0 until l.length
    } yield {
      println(x)
      println(l(x).move)
    }
    //board.drawBoard()
    //println("Make a move (Correct example: 3 2 4 3)")
    val in1 = scala.io.StdIn.readInt()
    val in2 = scala.io.StdIn.readInt()
    val in3 = scala.io.StdIn.readInt()
    val in4 = scala.io.StdIn.readInt()
    if (!board.move(in1, in2, in3, in4))
      println("Error")
  }
}
