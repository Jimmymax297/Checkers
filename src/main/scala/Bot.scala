class Bot(board_ : Board, color_ : Int = Checkers.Value.black) extends Player(board_ , color_ ) {
  /**
    * Decision of next bot move
    * @param movements possible list of movements
    * @param max - maximum number of actions
    * @return chosen move
    */
  override def chosenMovement(movements:List[Movement] = null, max : Int = 1): Movement = {
    val alpha = -100
    val beta = 100
    val depth = 6
    NegaScout(board, new Movement(List[(Int,Int,Int,Int)]()), color, alpha , beta, depth)._1
  }

  /**
    * Recursive function for finding best move with NegaScout algorithm
    * @param brd - board in this state
    * @param m - movement, that lead to this state
    * @param turn - colour of player having their torn in tis state
    * @param a - alpha parameter (maximized)
    * @param b - beta parameter (minimized)
    * @param d - depth of search
    * @return pair ( best movement from the subtree of the state , move value)
    */
  private def NegaScout(brd :Board, m : Movement, turn : Int, a : Int, b :Int, d : Int):(Movement, Int) = {
    if(d > 0){
      val possibleStrikes = brd.possibleStrikePaths(turn)
      lazy val possibleMoves = brd.findAllMovePaths(turn)
      val possibleActions = {
        if(possibleStrikes.nonEmpty)
          possibleStrikes
        else
          possibleMoves
      }

      val l = possibleActions.length
      if(l > 0) {
        if(turn == Checkers.Value.white)
          whiteTurn(brd, possibleActions, 0, l, turn, a, b, d)
        else
          blackTurn(brd, possibleActions, 0, l, turn, a, b, d)
      }
      else
        (m, brd.objectiveFunction)
    }
    else
      (m, brd.objectiveFunction)
  }

  /**
    * Turn for the white player. Is a part of NegaScout function. It checks deeper state subtree with scouting then with full check if necessary. It checks next neighbor if it exists o prune the rest.
    * @param brd - board in this state
    * @param pa - possible moves for all subtrees in called state
    * @param i - index of currently checked move
    * @param maxI - number of moves in this state
    * @param turn - colour of player having their torn in tis state
    * @param a - alpha parameter
    * @param b - beta parameter, possibly updated
    * @param d - search depth
    * @return ( move leading to this state or to neighbor , state value or neighbor's value)
    */
  private def whiteTurn(brd :Board, pa :List[Movement], i : Int, maxI : Int, turn: Int,a : Int, b :Int, d : Int):(Movement, Int) = {
    val newBoard = brd.copyBoard()
    newBoard.executeMovement(pa(i))

    val scoutScore = {
      if (i == 0)
        NegaScout(newBoard, pa(i), -turn, a, b, d - 1)
      else
        NegaScout(newBoard, pa(i), -turn, b - 1, b, d - 1)
    }

    val score = {
      if (i != 0 && a < scoutScore._2 && scoutScore._2 < b)
        NegaScout(newBoard, pa(i), -turn, a,b, d - 1)
      else
        scoutScore
    }

    val alpha = a
    val beta = b min score._2
    if(alpha <= beta && i + 1 < maxI){
      val neighbor =  whiteTurn(brd,pa,i+1,maxI,turn, alpha, beta ,d)

      if(neighbor._2 < score._2)
        neighbor
      else
        (pa(i),score._2)
    }
    else
      (pa(i),score._2)
  }
  
  /**
    *  Turn for the black player. It's analogical to whiteTurn
    * @param brd - board in this state
    * @param pa - possible moves for all subtrees in called state
    * @param i - index of currently checked move
    * @param maxI - number of moves in this state
    * @param turn - colour of player having their torn in tis state
    * @param a - alpha parameter
    * @param b - beta parameter, possibly updated
    * @param d - search depth
    * @return ( move leading to this state or to neighbor , state value or neighbor's value)
    */
  private def blackTurn(brd :Board, pa :List[Movement], i : Int, maxI : Int, turn: Int, a : Int, b :Int, d : Int):(Movement, Int) = {
    val newBoard = brd.copyBoard()
    newBoard.executeMovement(pa(i))

    val scoutScore = {
      if (i == 0)
        NegaScout(newBoard, pa(i), -turn, a, b, d - 1)
      else
        NegaScout(newBoard, pa(i), -turn, a, a + 1, d - 1)
    }

    val score = {
      if (i != 0 && a < scoutScore._2 && scoutScore._2 < b)
        NegaScout(newBoard, pa(i), -turn, a, b, d - 1)
      else
        scoutScore
    }

    val alpha = a max score._2
    val beta = b

    if(alpha <= beta && i + 1 < maxI){
      val neighbor =  blackTurn(brd,pa,i+1,maxI,turn,alpha, beta ,d)
      if(neighbor._2 > score._2)
        neighbor
      else
        (pa(i),score._2)
    }
    else
      (pa(i),score._2)
  }
}


