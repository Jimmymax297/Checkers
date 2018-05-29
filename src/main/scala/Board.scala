import java.lang.Cloneable

class Board(t: Array[Array[Int]]) extends Cloneable{
  val white: Int = -1
  val empty: Int = 0
  val black: Int = 1
  val whiteKing: Int = -2
  val blackKing: Int = 2
  val tab:Array[Array[Int]] = t

  def isChecker(x: Int, y: Int): Boolean = {
    if(tab(x)(y) == white || tab(x)(y) == black)
      true
    else
      false
  }

  def value(x: Int, y: Int): Int = tab(x)(y)

  def change(x: Int, y: Int, value: Int): Unit = tab(x)(y) = value

  def king(x: Int, y: Int): Unit = {
    if(isChecker(x, y))
      if(value(x, y) == black)
        change(x, y, blackKing)
      else
        change(x, y, whiteKing)
  }

  def color(x: Int, y: Int): Int = {
    if (value(x, y) == empty)
      empty
    else {
      if (value(x, y) == white || value(x, y) == whiteKing)
        white
      else
        black
    }
  }

  def direction(x_s: Int, y_s: Int, x_e: Int, y_e: Int, v: Int): Int = {
    if (x_e + v == x_s && y_e - v == y_s)
      0//left up
    else if (x_e - v == x_s && y_e - v == y_s)
      1//right up
    else if (x_e - v == x_s && y_e + v == y_s)
      2//right down
    else
      3//left down
  }

  def moveChecker(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (canMove(x_s, y_s, x_e, y_e) && math.abs(y_e - y_s) == 1 && canMoveForward(x_s, y_s, x_e, y_e)) {
      val value = tab(x_s)(y_s)
      tab(x_e)(y_e) = value
      tab(x_s)(y_s) = empty
    }
  }

  def canMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if ((x_s == x_e || y_s == y_e) ||
        value(x_e, y_e) != empty || math.abs(y_s - y_e) != math.abs(x_s - x_e))
      false
    else
      true
  }

  def canMoveForward(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (canMove(x_s, y_s, x_e, y_e) && value(x_s, y_s) == white && x_e < x_s && (y_e < y_s || y_e > y_s))
        true
    else if (canMove(x_s, y_s, x_e, y_e) && value(x_s, y_s) == black && x_e > x_s && (y_e < y_s || y_e > y_s))
      true
    else
        false
  }

  def canStrikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s) && canMove(x_s, y_s, x_e, y_e) && value((x_s + x_e) / 2, (y_s + y_e) / 2) == -value(x_s, y_s)) {
      true
    }
    else
      false
  }

  def strikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (canStrikeOnce(x_s, y_s, x_e, y_e)) {
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = empty
      tab((x_s + x_e) / 2)((y_s + y_e) / 2) = empty

    }
  }

  def canStrike(x_s: Int, y_s: Int): Boolean = {
    val v = value(x_s, y_s)
    if(
      ((x_s - 1 >=0 && y_s + 1 <= 7 && x_s - 2 >= 0 && y_s + 2 <= 7)&&
        (value(x_s - 1, y_s + 1) == -v && value(x_s - 2, y_s + 2) == empty)) //lewo dol
      ||
      ((x_s + 1 <= 7 && y_s + 1 <= 7 && x_s + 2 <= 7 && y_s + 2 <= 7)&&
        (value(x_s + 1, y_s + 1) == -v && value(x_s + 2, y_s + 2) == empty)) //prawo dol
      ||
      ((x_s + 1 <= 7 && y_s - 1 >= 0 && x_s + 2 <= 7 && y_s - 2 >= 0)&&
        (value(x_s + 1, y_s - 1) == -v && value(x_s + 2, y_s - 2) == empty)) //prawo gora
      ||
      ((x_s - 1 >= 0 && y_s - 1 >= 0 && x_s - 2 >= 0 && y_s - 2 >= 0)&&
        (value(x_s - 1, y_s - 1) == -v && value(x_s - 2, y_s - 2) == empty)) //lewo gora
    )
      true
    else
      false
  }

  def strikers(c: String): IndexedSeq[(Int, Int)] = {
    val col = if (c == "white")
      white
    else
      black
    val t: IndexedSeq[IndexedSeq[(Int, Int)]] = for {
      x <- 0 to 7
    } yield {
      for {
       y <- 0 to 7
      } yield {
        if((isChecker(x, y) || isKing(x, y)) && canStrike(x, y) && col == color(x, y))
          (x, y)
        else
          (-1, -1)
      }
    }
    t.flatten.filter(_ != (-1, -1))
  }

  def findAllStrikePaths(color_ : Int): List[Movement] = {
    val color = color_
    val movements = for {
      i <- 0 until 8
    } yield {
      for {
        j <- 0 until 8
        if j != i
      } yield {
        if (value(i , j) == color)
          findStrikePath(i, j, new Movement(List[(Int, Int, Int, Int)]()), this)
        else
          List[Movement]()
      }
    }
    movements.toList.flatten.flatten
  }

  def findAllKingStrikePaths(c: Int): List[Movement] = {
    val movements = for {
      i <- 0 until 8
    } yield {
      for {
      j <- 0 until 8
      if j != i
    } yield {
     if (isKing(i, j) && color(i, j) == c)
       findStrikePathKing(i, j, new Movement(List[(Int, Int, Int, Int)]()), this)
     else
       List[Movement]()
      }
    }
    movements.toList.flatten.flatten
  }

  def findAllKingMovePaths(c : Int): List[Movement] = {
    val movements = for {
      i <- 0 until 8
      j <- 0 until 8
      if j != i
    } yield {
        if (isKing(i, j) && color(i, j) == c)
          findMovePathKing(i, j, new Movement(List[(Int, Int, Int, Int)]()), this)
        else
          List[Movement]()
      }
    movements.toList.flatten.flatten
  }

  def possibleKingMovePaths(c: Int): List[Movement] = {
    val l = findAllKingMovePaths(c)
    val lengthList = l.map(_.move.length)
    if (lengthList.nonEmpty) {
      val maxLength = lengthList.max
      l.filter(_.move.length == maxLength)
    }
    else
      List[Movement]()
  }

  def possibleKingStrikePaths(c: Int): List[Movement] = {
    val l = findAllKingStrikePaths(c)
    val lengthList = l.map(_.move.length)
    if (lengthList.nonEmpty) {
      val maxLength = lengthList.max
      l.filter(_.move.length == maxLength)
    }
    else
      List[Movement]()
  }

  def possibleStrikePaths(s: Int): List[Movement] = {
    val l = findAllStrikePaths(s)
    val lengthList = l.map(_.move.length)
    if (lengthList.nonEmpty) {
      val maxLength = lengthList.max
      l.filter(_.move.length == maxLength)
    }
    else {
      List[Movement]()
    }
  }

  def findMovePath(x: Int, y: Int): List[Movement] = {
    val list = List[Movement](
      if (isChecker(x, y)
        && x - 1 >= 0 && y + 1 <= 7
        && canMoveForward(x, y, x - 1, y + 1)) {
          new Movement(List((x, y, x - 1, y + 1)))
        }
        else
          new Movement(List()),
      if (isChecker(x, y)
        &&x + 1 <= 7 && y + 1 <= 7
        && canMoveForward(x, y, x + 1, y + 1)) {
          new Movement(List((x, y, x + 1, y + 1)))
        }
        else
          new Movement(List()),
      if (isChecker(x, y)
        &&x + 1 <= 7 && y - 1 >= 0
        && canMoveForward(x, y, x + 1, y - 1)) {
          new Movement(List((x, y, x + 1, y - 1)))
        }
        else
          new Movement(List()),
    if (isChecker(x, y)
        && x - 1 >= 0 && y - 1 >= 0
        && canMoveForward(x, y, x - 1, y - 1)) {
          new Movement(List((x, y, x - 1, y - 1)))
        }
        else
          new Movement(List())
    )
    list
  }

  def findAllMovePaths(color_ : Int): List[Movement] = {
    val color = color_
    val movements = for {
      i <- 0 until 8
    } yield {
      for {
        j <- 0 until 8
        if j != i
      } yield {
        if (value(i , j) == color)
          findMovePath(i, j)
        else
          List[Movement]()
      }
    }
    movements.toList.flatten.flatten.filter(_.move.nonEmpty)
  }

  def copyBoard(b: Board): Board = {
    val newTab = Array(
      Array(empty, b.tab(0)(1), empty, b.tab(0)(3), empty, b.tab(0)(5), empty, b.tab(0)(7)),
      Array(b.tab(1)(0), empty, b.tab(1)(2), empty, b.tab(1)(4), empty, b.tab(1)(6), empty),
      Array(empty, b.tab(2)(1), empty, b.tab(2)(3), empty, b.tab(2)(5), empty, b.tab(2)(7)),
      Array(b.tab(3)(0), empty, b.tab(3)(2), empty, b.tab(3)(4), empty, b.tab(3)(6), empty),
      Array(empty, b.tab(4)(1), empty, b.tab(4)(3), empty, b.tab(4)(5), empty, b.tab(4)(7)),
      Array(b.tab(5)(0), empty, b.tab(5)(2), empty, b.tab(5)(4), empty, b.tab(5)(6), empty),
      Array(empty, b.tab(6)(1), empty, b.tab(6)(3), empty, b.tab(6)(5), empty, b.tab(6)(7)),
      Array(b.tab(7)(0), empty, b.tab(7)(2), empty, b.tab(7)(4), empty, b.tab(7)(6), empty))
    new Board(newTab)
  }

  def findStrikePath(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    if(b.value(x,y)==0)
      return List[Movement](m)

    val list = List[Movement](m) ++ findStrikeLeftDown(x,y,m,b) ++ findStrikeRightDown(x,y,m,b) ++ findStrikeRightUp(x,y,m,b) ++ findStrikeLeftUp(x,y,m,b)
    list.filter(_.move.nonEmpty)
  }

  def findStrikeLeftDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    if ((x - 1 >=0 && y + 1 <= 7 && x - 2 >= 0 && y + 2 <= 7)&&
      (b.value(x - 1, y + 1) == -v && b.value(x - 2, y + 2) == empty)) {
      val board = copyBoard(b)
      board.move(x, y, x - 2, y + 2)
      findStrikePath(x - 2, y + 2, new Movement(m.move :+ (x, y, x - 2, y + 2)), board)
    }
    else {
      List[Movement]()
    }
  }

  def findStrikeRightDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    if ((x + 1 <= 7 && y + 1 <= 7 && x + 2 <= 7 && y + 2 <= 7)&&
      (b.value(x + 1, y + 1) == -v && b.value(x + 2, y + 2) == empty)) {
      val board = copyBoard(b)
      board.move(x, y, x + 2, y + 2)
      findStrikePath(x + 2, y + 2, new Movement(m.move :+ (x, y, x + 2, y + 2)), board)
    }
    else{
      List[Movement]()
    }
  }

  def findStrikeRightUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    if ((x + 1 <= 7 && y - 1 >= 0 && x + 2 <= 7 && y - 2 >= 0)&&
      (b.value(x + 1, y - 1) == -v && b.value(x + 2, y - 2) == empty)) {
      val board = copyBoard(b)
      board.move(x, y, x + 2, y - 2)
      findStrikePath(x + 2, y - 2, new Movement(m.move :+ (x, y, x + 2, y - 2)), board)
    }
    else{
      List[Movement]()
    }
  }

  def findStrikeLeftUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    if ((x - 1 >= 0 && y - 1 >= 0 && x - 2 >= 0 && y - 2 >= 0)&&
      (b.value(x - 1, y - 1) == -v && b.value(x - 2, y - 2) == empty)) {
      val board = copyBoard(b)
      board.move(x, y, x - 2, y - 2)
      findStrikePath(x - 2, y - 2, new Movement(m.move :+ (x, y, x - 2, y - 2)), board)
    }
    else{
      List[Movement]()
    }
  }

  def isKing(x: Int, y: Int): Boolean = {
    value(x, y) == whiteKing || value(x, y) == blackKing
  }

  def canKingMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (!canMove(x_s, y_s, x_e, y_e))
      return false
    val inc = math.abs(y_e - y_s)
    val inc2 = math.abs(x_e - x_s)
    val dir = direction(x_s, y_s, x_e, y_e, inc)
    val res = for{
      i <- 1 until inc
      if inc == inc2
    } yield {
      if (dir == 0)
        onTheWay(x_s, y_s, x_s - i, y_s + i) //prawo gora
      else if (dir == 1)
        onTheWay(x_s, y_s, x_s + i, y_s + i) //prawo dol
      else if (dir == 2)
        onTheWay(x_s, y_s, x_s + i, y_s - i) //lewo dol
      else
        onTheWay(x_s, y_s, x_s - i, y_s - i) //lewo gora
    }
    if (res.sum == 0) {
      true
    }
    else
      false
  }

  def onTheWay(x: Int, y: Int, cx: Int, cy: Int): Int = {
    if (value(cx, cy) == empty) {
      0
    }
    else if ((value(x,y) == blackKing && (value(cx,cy) == whiteKing || value(cx,cy) == white))||
      value(x,y) == whiteKing && (value(cx,cy) == blackKing || value(cx,cy) == black)) {
      1
    }
    else {
      2
    }
  }

  def canKingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (!canMove(x_s, y_s, x_e, y_e)) {
      return false
    }
    val inc = math.abs(y_e - y_s)
    val dir = direction(x_s, y_s, x_e, y_e, inc)
    val res = for{
      i <- 1 until inc
      if math.abs(y_e - y_s) == math.abs(x_e - x_s)
    } yield {
      if (dir == 0)
        onTheWay(x_s, y_s, x_s - i, y_s + i)
      else if (dir == 1)
        onTheWay(x_s, y_s, x_s + i, y_s + i)
      else if (dir == 2)
        onTheWay(x_s, y_s, x_s + i, y_s - i)
      else
        onTheWay(x_s, y_s, x_s - i, y_s - i)
    }
    res.sum == 1
  }

  def findMovePathKing(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    if (b.value(x, y) == 0)
      return List[Movement](m)

    val list = List[Movement](m) ++ findMoveKingRightUp(x, y, m, b) ++ findMoveKingRightDown(x, y, m, b) ++ findMoveKingLeftDown(x, y, m, b) ++ findMoveKingLeftUp(x, y, m, b)
    list.filter(_.move.nonEmpty)
  }

  def findMoveKingRightUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8 - math.min(x, y)
      if x - i >= 0 && y + i <= 7
    } yield {
      if (b.canKingMove(x, y, x - i, y + i)) {
        val board = copyBoard(b)
        board.move(x, y, x - i, y + i)
        board.drawBoard()
        findMovePathKing(x - i, y + i, new Movement(m.move :+ (x, y, x - i, y + i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findMoveKingRightDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8 - math.min(x, y)
      if x + i <= 7 && y + i <= 7
    } yield {
      if (b.canKingMove(x, y, x + i, y + i)) {
        val board = copyBoard(b)
        board.move(x, y, x + i, y + i)
        board.drawBoard()
        findMovePathKing(x + i, y + i, new Movement(m.move :+ (x, y, x + i, y + i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findMoveKingLeftDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8 - math.min(x, y)
      if x + i <= 7 && y - i >= 0
    } yield {
      if (b.canKingMove(x, y, x + i, y - i)) {
        val board = copyBoard(b)
        board.move(x, y, x + i, y - i)
        board.drawBoard()
        findMovePathKing(x + i, y - i, new Movement(m.move :+ (x, y, x + i, y - i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findMoveKingLeftUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8 - math.min(x, y)
      if x - i >= 0 && y - i >= 0
    } yield {
      if (b.canKingMove(x, y, x - i, y - i)) {
        val board = copyBoard(b)
        board.move(x, y, x - i, y - i)
        board.drawBoard()
        findStrikePathKing(x - i, y - i, new Movement(m.move :+ (x, y, x - i, y - i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findStrikePathKing(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    if (b.value(x,y)==0)
      return List[Movement](m)

    val list = List[Movement](m) ++ findStrikeKingLeftDown(x,y,m,b) ++ findStrikeKingRightDown(x,y,m,b) ++ findStrikeKingRightUp(x,y,m,b) ++ findStrikeKingLeftUp(x,y,m,b)
    list.filter(_.move.nonEmpty)
  }

  def findStrikeKingRightUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8 - math.min(x, y)
      if x - i >= 0 && y + i <= 7
    } yield {
      if (b.canKingStrike(x, y, x - i, y + i)) {
        val board = copyBoard(b)
        board.move(x, y, x - i, y + i)
        board.drawBoard()
        findStrikePathKing(x - i, y + i, new Movement(m.move :+ (x, y, x - i, y + i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findStrikeKingRightDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until (8 - math.min(x, y))
      if x + i <= 7 && y + i <= 7
    } yield {
      if (b.canKingStrike(x, y, x + i, y + i)) {
        val board = copyBoard(b)
        board.move(x, y, x + i, y + i)
        board.drawBoard()
        findStrikePathKing(x + i, y + i, new Movement(m.move :+ (x, y, x + i, y + i)), board)
      }
      else {
        List[Movement]()
      }
    }
    res.toList.flatten
  }

  def findStrikeKingLeftDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until (8 - math.min(x, y))
      if x + i <= 7 && y - i >= 0
        if b.canKingStrike(x, y, x + i, y - i)
    } yield {
      if (b.canKingStrike(x, y, x + i, y - i)) {
        val board = copyBoard(b)
        board.move(x, y, x + i, y - i)
        board.drawBoard()
        findStrikePathKing(x + i, y - i, new Movement(m.move :+ (x, y, x + i, y - i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findStrikeKingLeftUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until (8 - math.min(x, y))
      if x - i >= 0 && y - i >= 0
    } yield {
      if (b.canKingStrike(x, y, x - i, y - i)) {
        val board = copyBoard(b)
        board.move(x, y, x - i, y - i)
        board.drawBoard()
        findStrikePathKing(x - i, y - i, new Movement(m.move :+ (x, y, x - i, y - i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def moveKing(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isKing(x_s, y_s) && canKingMove(x_s, y_s, x_e, y_e)){
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = empty
    }
  }

  def kingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    val inc = math.abs(y_e - y_s)
    val dir = direction(x_s, y_s, x_e, y_e, inc)
    if (isKing(x_s, y_s) && canKingStrike(x_s, y_s, x_e, y_e)) {
      for {
        i <- 1 until inc
      } yield {
        if (dir == 0)
          tab(x_s - i)(y_s + i) = 0 //prawo gora
        else if (dir == 1)
          tab(x_s + i)(y_s + i) = 0 //prawo dol
        else if (dir == 2)
          tab(x_s + i)(y_s - i) = 0 //lewo dol
        else
          tab(x_s - i)(y_s - i) = 0//lewo gora
      }
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = empty
    }
  }

  def move(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s))
      if (math.abs(x_s - x_e) == 2) {
        strikeOnce(x_s, y_s, x_e, y_e)
        true
      }
      else {
        moveChecker(x_s, y_s, x_e, y_e)
        true
      }
    else if (isKing(x_s, y_s)) {
      if (canKingStrike(x_s, y_s, x_e, y_e)) {
        kingStrike(x_s, y_s, x_e, y_e)
        true
      }
      else {
        moveKing(x_s, y_s, x_e, y_e)
        true
      }
    }
    else{
      false
    }
  }

  def executeMovement(m: Movement): Unit = {
    val len =  m.move.length
    for {
      i <- 0 until len
    } yield {
      val x1 = m.move(i)._1
      val y1 = m.move(i)._2
      val x2 = m.move(i)._3
      val y2 = m.move(i)._4
      move(x1,y1,x2,y2)
    }
  }

  def printing(x: Int, y: Int): Any = {
    if (value(x, y) == 0)
      '-'
    else if (value(x, y) == black)
      'c'
    else if (value(x, y) == blackKing)
      'C'
    else if (value(x, y) == white)
      'b'
    else if (value(x, y) == whiteKing)
      'B'
  }

  def drawBoard(): Unit = {
    println("x/y  0    1    2    3    4    5    6    7\n")
    for {
      i <- 0 until 8
    } yield {
      print(i)
      print("    ")
      for {
        j <- 0 until 8
      } yield {
        print(printing(i, j) + "    ")
        if (j == 7)
          println("\n")
      }
    }
  }

  def objectiveFunction():Int = tab(0).sum + tab(1).sum + tab(2).sum + tab(3).sum + tab(4).sum + tab(5).sum + tab(6).sum+ tab(7).sum
}