/**
  * Main object of project Checkers, runs the whole game.
  */
object Checkers extends App {

  /**
    * enum of Values on board
    */
  object Value extends Enumeration{
    val white: Int = -1
    val empty: Int = 0
    val black: Int = 1
    val whiteKing: Int = -3
    val blackKing: Int = 3
  }

  object GameState{
    val turnColor = 0
    val score = 1
    val possibleMoves = 2
    val noStrikeKingMoves = 3
    val stats = Array(Value.white, 0, 7, 0)

    def nextTurn(): Unit = {
      if(stats(turnColor) == Value.white)
        stats(turnColor) = Value.black
      else
        stats(turnColor) = Value.white
      stats(score) = board.objectiveFunction
      if(stats(noStrikeKingMoves) >= 30){
        stats(possibleMoves) = 0
      }
    }
  }

  val boardFields = Array(
    Array(Value.empty, Value.black, Value.empty, Value.black, Value.empty, Value.black, Value.empty, Value.black),
    Array(Value.black, Value.empty, Value.black, Value.empty, Value.black, Value.empty, Value.black, Value.empty),
    Array(Value.empty, Value.black, Value.empty, Value.black, Value.empty, Value.black, Value.empty, Value.black),
    Array(Value.empty, Value.empty, Value.empty, Value.empty, Value.empty, Value.empty, Value.empty, Value.empty),
    Array(Value.empty, Value.empty, Value.empty, Value.empty, Value.empty, Value.empty, Value.empty, Value.empty),
    Array(Value.white, Value.empty, Value.white, Value.empty, Value.white, Value.empty, Value.white, Value.empty),
    Array(Value.empty, Value.white, Value.empty, Value.white, Value.empty, Value.white, Value.empty, Value.white),
    Array(Value.white, Value.empty, Value.white, Value.empty, Value.white, Value.empty, Value.white, Value.empty))

  val board =  new Board(boardFields)

  val playerColor = choosePlayerColor
  val player = new Player(board,playerColor)
  val bot = new Bot(board,oppositeColor(playerColor))

  while (GameState.stats(GameState.possibleMoves)!=0) {
    board.drawBoard(playerColor)
    if(GameState.stats(GameState.turnColor) == player.color){
      print("Turn: player , ")
      if(player.color == Value.white)
        println("white")
      else
        println("black")
      chooseMove(player,player.color)
    }else {
      print("Turn bot , ")
      if(bot.color == Value.white)
        println("white")
      else
        println("black")
      chooseMove(bot,bot.color)
    }
    GameState.nextTurn()
  }

  gameOver()

  /**
    * Funtion which reads movements from actual board state for current player
    * @param player_ - player or bot
    * @param c - color of player
    */
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
        possibleStrike(i).print()
      }
      val chosenMove = player_.chosenMovement(possibleStrike,len)
      print("chosen : ")
      chosenMove.print()
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
          possibleMove(i).print()
        }
        val chosenMove = player_.chosenMovement(possibleMove,len)
        print("chosen : ")
        chosenMove.print()
        if(board.executeMovement(chosenMove))
          GameState.stats(GameState.noStrikeKingMoves) += 1
      }
      else {
        GameState.stats(GameState.possibleMoves) = 0
      }
    }else
      GameState.stats(GameState.possibleMoves) = 0
  }

  /**
    * Function to pick color at the beginning of the game
    * @return int of chosen color or exit
    */
  def choosePlayerColor: Int = {
    println("Choose color (white/black) or exit")
    println("1. white")
    println("2. black")
    println("3. exit")
    try{
      val read = scala.io.StdIn.readInt()
      if(read == 1)
        Value.white
      else if(read == 2)
        Value.black
      else if(read == 3){
        System.exit(1)
        -1
      }
      else
        throw new IllegalArgumentException("wrong input")
    }catch{
      case e: Exception =>
        println("please enter number of one of possible options")
        choosePlayerColor
    }
  }

  def gameOver(): Unit = {
    println("Game Over")
    val score = GameState.stats(GameState.score)
    if(score < 0){
      if(playerColor == Value.white)
        println("Player wins!")
      else
        println("IA wins!")
      println("score: " + -score)
    }
    else if(score > 0){
      if(playerColor == Value.white)
        println("AI wins!")
      else
        println("Player wins!")
      println("score: " + score)
    }
    else {
      println("Draw!")
      if(GameState.stats(GameState.noStrikeKingMoves)>=30)
        println("Over 15 king's moves without striking")
    }
  }

  /**
    * Function to return opposing color
    * @param color color to return opposing from
    * @return opposing color
    */
  def oppositeColor(color: Int): Int ={
    if(color == Value.white)
      Value.black
    else if(color == Value.black)
      Value.white
    else
      throw new NoSuchFieldException("Unknown color")
  }

}
