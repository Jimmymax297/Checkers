object Checkers extends App {
  //declartion and definitions
  val white: Int = -1
  val empty: Int = 0
  val black: Int = 1
  val whiteKing: Int = -2
  val blackKing: Int = 2

  object GameState{
    //val playerColor = 0
    val turnColor = 0
    val score = 1
    val possibleMoves = 2
    val stats = Array(/*white,*/ white, 0, 7)

    def nextTurn: Unit = {
      if(stats(turnColor) == white)
        stats(turnColor) = black
      else
        stats(turnColor) = white
      stats(score) = board.objectiveFunction
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
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, white, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, blackKing),
    Array(empty, empty, empty, empty, empty, empty, empty, empty)
  )
  val board =  new Board(boardFields)
  //board.drawBoard()
  /*val r = board.possibleKingStrikePaths(black)
  val len = r.length
  for {
    i <- 0 until len
  } yield {
    println(r(i).move)
  }*/

  try {
    //input player color
    val playerColor = choosePlayerColor
    val player = new Player(board,playerColor)
    val bot = new Bot(board,oppositeColor(playerColor))
    println(player.color)
    println(bot.color)
    //main loop
    while (GameState.stats(GameState.possibleMoves)!=0) {
      board.drawBoard()
      //println("*")
      if(GameState.stats(GameState.turnColor) == player.color){
        print("Turn: player , ")
        if(player.color == white) {
          println("white")
          chooseMove(player,white)
        }
        else {
          println("black")
          chooseMove(player,black)
        }
      }else {
        print("Turn bot , ")

        if(bot.color == white){
          println("white")
          chooseMove(bot,white)
        }
        else{
          println("black")
          chooseMove(bot,black)
        }
      }
      GameState.nextTurn
    }
    println("Game Over")
    val score = GameState.stats(GameState.score)
    if(score < 0){
      println("Player wins!")
      println("score: " + -score)
    }
    else if(score > 0){
      println("IA wins!")
      println("score: " + score)
    }
    else
      println("Draw!")

  }catch {
    case e: Exception => println("error: " + e.getMessage)
      System.exit(1)
  }

  def chooseMove(player_ : Player,c: Int): Unit = {
    val possibleStrike = board.possibleStrikePaths(c) ++ board.possibleKingStrikePaths(c)
    val possibleMove = board.findAllMovePaths(c) ++ board.findAllKingMovePaths(c)
    if (possibleStrike.nonEmpty) {
      GameState.stats(GameState.possibleMoves) = possibleStrike.length
      println("Possible strikes:")
      val len = possibleStrike.length
      for {
        x <- 0 until len
      } yield {
        println(x + 1 + ":  " + possibleStrike(x).move)
      }
      val chosenMove = player_.chosenMovement(possibleStrike)
      board.executeMovement(chosenMove)
      }
    else if (possibleMove.nonEmpty) {
      GameState.stats(GameState.possibleMoves) = possibleMove.length
      println("Possible moves:")
      val len = possibleMove.length
      for {
        x <- 0 until len
      } yield {
        println(x + 1 + ": " + possibleMove(x).move)
      }
      val chosenMove = player_.chosenMovement(possibleMove)
      board.executeMovement(chosenMove)
    }else
      GameState.stats(GameState.possibleMoves) = 0
  }

  def choosePlayerColor: Int = {
    println("Choose color (white/black) or exit")
    println("1. white")
    println("2. black")
    println("3. exit")
    val read = scala.io.StdIn.readInt()
    if(read == 1)
      white
    else if(read == 2)
      black
    else if(read == 3){
      System.exit(1)
      -1
    }
    else
      throw new IllegalArgumentException("wrong input")
  }

  def oppositeColor(color: Int): Int ={
    if(color == white)
      black
    else if(color == black)
      white
    else
      throw new NoSuchFieldException("Unknown color")
  }

}
