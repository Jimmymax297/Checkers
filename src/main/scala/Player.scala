class Player(board_ : Board, color_ : Int = Checkers.white) {
  val color = color_
  val board = board_

  def chosenMovement(movements:List[Movement]): Movement = {
      val chosenMovement = scala.io.StdIn.readInt()
      movements(chosenMovement - 1)
  }
}
