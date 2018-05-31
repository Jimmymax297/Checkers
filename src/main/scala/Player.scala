class Player(board_ : Board, color_ : Int = Checkers.Value.white) {
  val color :Int = color_
  protected val board : Board= board_

   def chosenMovement(movements:List[Movement],max :Int = 1): Movement = {
      try {
        val chosenMovement = scala.io.StdIn.readInt()
        movements(chosenMovement - 1)
      }catch{
        case e: Exception =>
          println("please enter number of one of possible moves/strikes")
          chosenMovement(movements,max)
      }
  }
}
