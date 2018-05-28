object Checkers extends App {
  //declartion and definitions
  val white: Int = -1
  val empty: Int = 0
  val black: Int = 1
  val whiteKing: Int = -2
  val blackKing: Int = 2

  object GameState{
    val playerColor = 0
    val turnColor = 1
    val score = 2
    val stats = Array(white,white, 0)

    def nextTurn = {
      if(stats(GameState.turnColor) == white)
        stats(GameState.turnColor) = black
      else
        stats(GameState.turnColor) = white
      stats(GameState.score) = board.objectiveFunction
    }
  }

  //init
  /*val boardFields = Array(
    Array(empty, black, empty, black, empty, black, empty, black),
    Array(black, empty, black, empty, black, empty, black, empty),
    Array(empty, black, empty, black, empty, black, empty, black),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(white, empty, white, empty, white, empty, white, empty),
    Array(empty, white, empty, white, empty, white, empty, white),
    Array(white, empty, white, empty, white, empty, white, empty))*/
  val boardFields = Array(
    Array(empty, black, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, white, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, white, empty, white, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, white, empty, white, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty)
  )
  val board =  new Board(boardFields)


  try {
    //input player color
    val isPlayerWhite = choosePlayerColor
    if(isPlayerWhite)
      GameState.stats(GameState.turnColor) = white
    else
      GameState.stats(GameState.turnColor) = black
    //debug
    GameState.stats(GameState.playerColor) = white
    GameState.stats(GameState.score) = board.objectiveFunction
    //

    //main loop
    while (true) {
      board.drawBoard()
      if(GameState.stats(GameState.turnColor) == GameState.stats(GameState.playerColor)){
        println("turn: player")
        val in1 = scala.io.StdIn.readInt()
        val in2 = scala.io.StdIn.readInt()
        val in3 = scala.io.StdIn.readInt()
        val in4 = scala.io.StdIn.readInt()
        if (board.move(in1, in2, in3, in4)){
          GameState.nextTurn
        }else{
          println("invalid move")
        }
      }else {
        println("turn bot")
        val in1 = scala.io.StdIn.readInt()
        val in2 = scala.io.StdIn.readInt()
        val in3 = scala.io.StdIn.readInt()
        val in4 = scala.io.StdIn.readInt()
        if (board.move(in1, in2, in3, in4)) {
          GameState.nextTurn
        } else {
          println("invalid move")
        }
      }
    }

  }catch {
    case e: Exception => println("error: " + e.getMessage)
      System.exit(1)
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

  def choosePlayerColor: Boolean = {
    println("Choose color (white/black) or exit")
    val read = scala.io.StdIn.readLine()
    if(read == "white")
      true
    else if(read == "black")
      false
    else if(read == "exit"){
      System.exit(0)
      false
    }
    else
      throw new IllegalArgumentException("wrong input")
  }


}
