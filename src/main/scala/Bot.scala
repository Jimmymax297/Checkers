import java.awt.Color

class Bot(board_ : Board, color_ : Int = Checkers.white) extends Player(board_ , color_ ) {
  override def chosenMovement(movements:List[Movement]): Movement = {
    val alfa = -100
    val beta = 100
    val depth = 6
    negascout(board, new Movement(List[(Int,Int,Int,Int)]()), color, alfa , beta, depth)._1
    //val chosenMovement = scala.io.StdIn.readInt()
    //movements(chosenMovement - 1)
  }

  def negascout(brd :Board, m : Movement,turn : Int, a : Int, b :Int, d : Int):(Movement, Int) = {
    //reached the given depth
//    println("negascout")
//    println("turn " + turn)
//    println("alpha: " + a + "beta: " + b)
//    println("depth: " + d)
//    brd.drawBoard
//    println("move")
//    m.print

    if(d > 0){
      //creating possible states, strikes are more important
      val possibleStricks = brd.possibleStrikePaths(turn)
      lazy val possibleMoves = brd.findAllMovePaths(turn)
      val possibleActions = {
        if(possibleStricks.nonEmpty)
          possibleStricks
        else
          possibleMoves
      }

      val l = possibleActions.length
//      println("no possible movements: " + l)
      if(l > 0) {
        if(turn == Checkers.white) {
          val ret = whiteTurn(brd, possibleActions, 0, l, turn, a, b, d)
//          println("negascout return")
//          ret._1.print
//          println(ret._2)
//          println("---")
          ret
        }else {
          val ret = blackTurn(brd, possibleActions, 0, l, turn, a, b, d)
//          println("negascout return")
//          ret._1.print
//          println(ret._2)
//          println("---")
          ret
        }
      }
      //no more moves
      else {
        val ret = (m, brd.objectiveFunction)
//        println("negascout return")
//        ret._1.print
//        println(ret._2)
//        println("---")
        ret
      }
    }
    else{
      val ret = (m, brd.objectiveFunction)
//      println("negascout return")
//      ret._1.print
//      println(ret._2)
//      println("---")
      ret
    }
  }

  def whiteTurn(brd :Board, pa :List[Movement], i : Int, maxI : Int, turn: Int,a : Int, b :Int, d : Int):(Movement, Int) = {
//    println("whiteturn")
//    println("turn " + turn)
//    println("alpha: " + a + "beta: " + b)
//    println("depth: " + d)

    val newBoard = brd.copyBoard()
    newBoard.executeMovement(pa(i))

//    brd.drawBoard
//    println("index " + i)
//    println("max " + maxI)
//    println("move")
//    pa(i).print

//    println("try scout")
    val scoutScore = {
      if (i == 0)
        negascout(newBoard, pa(i), -turn, a, b, d - 1)
      else
        negascout(newBoard, pa(i), -turn, b - 1, b, d - 1)
    }
//    println("scout")
//    scoutScore._1.print
//    println(scoutScore._2)

//    println("try score")
    val score = {
      if (i != 0 && a < scoutScore._2 && scoutScore._2 < b)
        negascout(newBoard, pa(i), -turn, a, b, d - 1)
      else
        scoutScore
    }
//    println("score")
//    score._1.print
//    println(score._2)

    val alpha = a
    val beta = b min score._2
//    println("alpha : " + alpha)
//    println("beta : " + beta + " := " + b + "min" + score._2 )
//    if(alpha > beta)
//      println("cut : alpha > beta")
//    println(i + 1 + " ? " + maxI)
    if(alpha <= beta
      && i + 1 < maxI){
//      println("call neighbor")
      val neighbor =  whiteTurn(brd,pa,i+1,maxI,turn, alpha, beta ,d)
      if(neighbor._2 < score._2) {
        val ret = neighbor
//        println("whiteturn return: neighbor")
//        ret._1.print
//        println(ret._2)
//        println("---")
        ret
      }else {
        val ret = (pa(i),score._2)
//        println("whiteturn return: self")
//        ret._1.print
//        println(ret._2)
//        println("---")
        ret
      }
    }
    else{
      val ret = (pa(i),score._2)
//      println("whiteturn return: self")
//      ret._1.print
//      println(ret._2)
//      println("---")
      ret
    }
  }

  def blackTurn(brd :Board, pa :List[Movement], i : Int, maxI : Int, turn: Int, a : Int, b :Int, d : Int):(Movement, Int) = {
//    println("blackturn")
//    println("turn " + turn)
//    println("alpha: " + a + "beta: " + b)
//    println("depth: " + d)

    val newBoard = brd.copyBoard()
    newBoard.executeMovement(pa(i))

//    brd.drawBoard
//    println("index " + i)
//    println("max " + maxI)
//    println("move")
//    pa(i).print

//    println("try scout")
    val scoutScore = {
      if (i == 0)
        negascout(newBoard, pa(i), -turn, a, b, d - 1)
      else
        negascout(newBoard, pa(i), -turn, a, a + 1, d - 1)
    }
//    println("scout")
//    scoutScore._1.print
//    println(scoutScore._2)

//    println("try score")
    val score = {
      if (i != 0 && a < scoutScore._2 && scoutScore._2 < b)
        negascout(newBoard, pa(i), -turn, a, b, d - 1)
      else
        scoutScore
    }
//    println("score")
//    score._1.print
//    println(score._2)

    val alpha = a max score._2
    val beta = b
//    println("alpha : " + alpha + " := " + a + "max" + score._2 )
//    println("beta : " + beta )
//    if(alpha > beta)
//      println("cut : alpha > beta")
//    println(i + 1 + " ? " + maxI)
    if(alpha <= beta
      && i + 1 < maxI){
//      println("call neighbor")
      val neighbor =  blackTurn(brd,pa,i+1,maxI,turn,alpha, beta ,d)
      if(neighbor._2 > score._2){
        val ret = neighbor
//        println("blackturn return: neighbor")
//        ret._1.print
//        println(ret._2)
//        println("---")
        ret
      }
      else{
        val ret = (pa(i),score._2)
//        println("blackturn return: self")
//        ret._1.print
//        println(ret._2)
//        println("---")
        ret}
    }
    else{
      val ret = (pa(i),score._2)
//      println("blackturn return; self")
//      ret._1.print
//      println(ret._2)
//      println("---")
      ret
    }
  }
}


