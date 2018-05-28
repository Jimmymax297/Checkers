object Checkers extends App {
  val white: Int = -1
  val empty: Int = 0
  val black: Int = 1
  val whiteKing: Int = -2
  val blackKing: Int = 2
  /*val t = Array(
    Array(empty, black, empty, black, empty, black, empty, black),
    Array(black, empty, black, empty, black, empty, black, empty),
    Array(empty, black, empty, black, empty, black, empty, black),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(white, empty, white, empty, white, empty, white, empty),
    Array(empty, white, empty, white, empty, white, empty, white),
    Array(white, empty, white, empty, white, empty, white, empty))*/
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

  println("Choose color (white/black")
  val read = scala.io.StdIn.readLine()

  while (true) {
    board.drawBoard()
    println(board.objectiveFunction())
    chooseMove(read)
    //println("White strikers:")
    /*val lw = board.biggestStrikePath("white")
    for {
      x <- 0 until lw.length
    } yield {
      println(lw(x).move)
    }
    println("Black robbers:")
    val nigger = board.biggestStrikePath("black")
    for {
      x <- 0 until nigger.length
    } yield {
      println(nigger(x).move)
    }*/
    //println(board.findAllMovePaths("black"))
    /*val in1 = scala.io.StdIn.readInt()
    val in2 = scala.io.StdIn.readInt()
    val in3 = scala.io.StdIn.readInt()
    val in4 = scala.io.StdIn.readInt()
    if (!board.move(in1, in2, in3, in4))
      println("Error")*/
  }

  def chooseMove(c: String): Unit = {
    val possibleStrike = board.biggestStrikePath(c)
    val possibleMove = board.findAllMovePaths(c)
    if (possibleStrike.nonEmpty) {
      println("Possible strikes:")
      val len = possibleStrike.length
      for {
        x <- 0 until len
      } yield {
        println(x + 1 + ":  " + possibleStrike(x).move)
        }
      val chosenMovement = scala.io.StdIn.readInt()
      //println(possibleStrike(chosenMovement - 1).move)
      board.executeMovement(possibleStrike(chosenMovement - 1))
      board.drawBoard()
      }
    else if (possibleMove.nonEmpty) {
      println("Possible moves:")
      val len = possibleMove.length
      for {
        x <- 0 until len
      } yield {
        println(x + 1 + ": " + possibleMove(x).move)
      }
      val chosenMove = scala.io.StdIn.readInt()
      board.executeMovement(possibleMove(chosenMove - 1))
      board.drawBoard()
    }
    }
}
