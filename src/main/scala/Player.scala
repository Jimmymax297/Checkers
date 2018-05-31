/**
  * Class Player - to check correct behavior of player
  * @param board_ - board state
  * @param color_ - color of player
  */
class Player(board_ : Board, color_ : Int = Checkers.Value.white) {
  val color :Int = color_
  protected val board : Board= board_

  /**
    * chosenMovement - function to read selected movement from user and check if it is valid
    * @param movements - list of movements presented to player
    * @param max - maximum number of movements presented to player
    * @return - chosen Movement
    */
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
