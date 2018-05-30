import java.awt.Color

class Bot(board_ : Board, color_ : Int = Checkers.white) extends Player(board_ , color_ ) {
  override def chosenMovement(movements:List[Movement]): Movement = {
    val alfa = -100
    val beta = 100
    val depth = 5
    negascout(board, new Movement(List[(Int,Int,Int,Int)]()), color, alfa , beta, depth)._1
    //val chosenMovement = scala.io.StdIn.readInt()
    //movements(chosenMovement - 1)
  }

  def negascout(brd :Board, m : Movement,turn : Int, a : Int, b :Int, d : Int):(Movement, Int) = {
    //reached the given depth
    if(d > 0){
      //creating possible states, strikes are more important
      val possibleStricks = brd.findAllStrikePaths(turn)
      lazy val possibleMoves = brd.findAllMovePaths(turn)
      val possibleActions = {
        if(possibleStricks.nonEmpty)
          possibleStricks
        else
          possibleMoves
      }

      possibleActions.sortBy(-_.objectiveFunction(brd))

      val l = possibleActions.length
      if(l > 0) {
        if(turn == Checkers.white)
          whiteTurn(brd, possibleActions,0,l,turn,a,b,d)
        else
          blackTurn(brd, possibleActions,0,l,turn,a,b,d)
      }
      //no more moves
      else
        (m, brd.objectiveFunction)
    }
    else
      (m, brd.objectiveFunction)
  }

  def whiteTurn(brd :Board, pa :List[Movement], i : Int, maxI : Int, turn: Int,a : Int, b :Int, d : Int):(Movement, Int) = {
    val newBoard = brd.copyBoard()
    newBoard.executeMovement(pa(i))
    if(a > b) {
      (pa(i),newBoard.objectiveFunction)
    }
    else{
      val scoutScore = {
        if (i == 0)
          negascout(newBoard, pa(i), -turn, a, b, d - 1)
        else
          negascout(newBoard, pa(i), -turn, b, b, d - 1)
      }

      val score = {
          if (i != 0 && a < scoutScore._2 && scoutScore._2 < b)
            negascout(newBoard, pa(i), -turn, a, scoutScore._2, d - 1)
          else
            scoutScore

      }

      if(i + 1 < maxI){
        val neighbor =  whiteTurn(brd,pa,i+1,maxI,turn,a, b min score._2 ,d)
        if(neighbor._2 < score._2)
          neighbor
        else
          (pa(i),score._2)
      }
      else
        (pa(i),score._2)
    }
  }

  def blackTurn(brd :Board, pa :List[Movement], i : Int, maxI : Int, turn: Int, a : Int, b :Int, d : Int):(Movement, Int) = {
    val newBoard = brd.copyBoard()
    newBoard.executeMovement(pa(i))
    if(a > b) {
      (pa(i),newBoard.objectiveFunction)
    }
    else{
      val scoutScore = {
        if (i == 0)
          negascout(newBoard, pa(i), -turn, a, b, d - 1)
        else
          negascout(newBoard, pa(i), -turn, a, a, d - 1)
      }

      val score = {
        if (i != 0 && a < scoutScore._2 && scoutScore._2 < b)
          negascout(newBoard, pa(i), -turn, scoutScore._2, b, d - 1)
        else
          scoutScore
      }

      if(i + 1 < maxI){
        val neighbor =  whiteTurn(brd,pa,i+1,maxI,turn,a max score._2, b ,d)
        if(neighbor._2 > score._2)
          neighbor
        else
          (pa(i),score._2)
      }
      else
        (pa(i),score._2)
    }
  }
}


