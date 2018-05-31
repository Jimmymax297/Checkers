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
 /* val boardFields = Array(
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, white, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, black),
    Array(empty, empty, empty, empty, empty, empty, empty, empty)
  )*/
  val boardFields = Array(
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, black, empty),
    Array(empty, black, empty, black, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, empty),
    Array(empty, empty, empty, empty, empty, empty, black, empty),
    Array(empty, empty, empty, empty, empty, empty, empty, whiteKing),
    Array(empty, empty, empty, empty, empty, empty, empty, empty)
  )
  val board =  new Board(boardFields)


  try {
    //input player color
    val playerColor = choosePlayerColor
    val player = new Player(board,playerColor)
    val bot = new Bot(board,oppositeColor(playerColor))
    //main loop
    while (GameState.stats(GameState.possibleMoves)!=0) {
      board.drawBoard(playerColor)
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
      if(playerColor == white)
        println("Player wins!")
      else
        println("IA wins!")
      println("score: " + -score)
    }
    else if(score > 0){
      if(playerColor == white)
        println("IA wins!")
      else
        println("Player wins!")
      println("score: " + score)
    }
    else
      println("Draw!")

  }catch {
    case e: Exception => println("error: " + e.getCause)
      System.exit(1)
  }

  def chooseMove(player_ : Player,c: Int): Unit = {
    val possibleStrike = board.possibleStrikePaths(c)
    val possibleMove = board.findAllMovePaths(c)
    if (possibleStrike.nonEmpty) {
      GameState.stats(GameState.possibleMoves) = possibleStrike.length
      println("Possible strikes:")
      val len = possibleStrike.length
      for {
        i<- 0 until len
      } yield {
        print(i + 1 + ":  ")
        possibleStrike(i).print
      }
      val chosenMove = player_.chosenMovement(possibleStrike)
      print("chosen : ")
      chosenMove.print
      board.executeMovement(chosenMove)
      }
    else if (possibleMove.nonEmpty) {
      val whiteAndBlack = board.countCheckers
      if(whiteAndBlack._1 > 0 && whiteAndBlack._2 > 0) {


        GameState.stats(GameState.possibleMoves) = possibleMove.length
        println("Possible moves:")
        val len = possibleMove.length
        for {
          i <- 0 until len
        } yield {
          print(i + 1 + ": ")
          possibleMove(i).print
        }
        val chosenMove = player_.chosenMovement(possibleMove)
        print("chosen : ")
        chosenMove.print
        board.executeMovement(chosenMove)
      }
      else {
        GameState.stats(GameState.possibleMoves) = 0
      }
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
