class Board(t: Array[Array[Int]]){
  val tab:Array[Array[Int]] = t

  def copyBoard(b: Board = this): Board = {
    val newTab = Array(
      Array(Checkers.Value.empty, b.tab(0)(1), Checkers.Value.empty, b.tab(0)(3), Checkers.Value.empty, b.tab(0)(5), Checkers.Value.empty, b.tab(0)(7)),
      Array(b.tab(1)(0), Checkers.Value.empty, b.tab(1)(2), Checkers.Value.empty, b.tab(1)(4), Checkers.Value.empty, b.tab(1)(6), Checkers.Value.empty),
      Array(Checkers.Value.empty, b.tab(2)(1), Checkers.Value.empty, b.tab(2)(3), Checkers.Value.empty, b.tab(2)(5), Checkers.Value.empty, b.tab(2)(7)),
      Array(b.tab(3)(0), Checkers.Value.empty, b.tab(3)(2), Checkers.Value.empty, b.tab(3)(4), Checkers.Value.empty, b.tab(3)(6), Checkers.Value.empty),
      Array(Checkers.Value.empty, b.tab(4)(1), Checkers.Value.empty, b.tab(4)(3), Checkers.Value.empty, b.tab(4)(5), Checkers.Value.empty, b.tab(4)(7)),
      Array(b.tab(5)(0), Checkers.Value.empty, b.tab(5)(2), Checkers.Value.empty, b.tab(5)(4), Checkers.Value.empty, b.tab(5)(6), Checkers.Value.empty),
      Array(Checkers.Value.empty, b.tab(6)(1), Checkers.Value.empty, b.tab(6)(3), Checkers.Value.empty, b.tab(6)(5), Checkers.Value.empty, b.tab(6)(7)),
      Array(b.tab(7)(0), Checkers.Value.empty, b.tab(7)(2), Checkers.Value.empty, b.tab(7)(4), Checkers.Value.empty, b.tab(7)(6), Checkers.Value.empty))
    new Board(newTab)
  }

  def printing(x: Int, y: Int): Any = {
    if (value(x, y) == 0)
      '-'
    else if (value(x, y) == Checkers.Value.black)
      'b'
    else if (value(x, y) == Checkers.Value.blackKing)
      'B'
    else if (value(x, y) == Checkers.Value.white)
      'w'
    else if (value(x, y) == Checkers.Value.whiteKing)
      'W'
  }

  def drawBoard(color : Int): Unit = {
    println("x/y  0    1    2    3    4    5    6    7\n")
    for {
      i <- 0 until 8
    } yield {
      if(color == Checkers.Value.white)
        print(i)
      else
        print(7-i)
      print("    ")
      for {
        j <- 0 until 8
      } yield {
        if(color == Checkers.Value.white)
          print(printing(i, j) + "    ")
        else
          print(printing(7 - i, j) + "    ")
        if (j == 7)
          println("\n")
      }
    }
  }

  def objectiveFunction:Int = {
    tab.map(_.toList).toList.flatten.sum
  }

  def countCheckers: (Int,Int) = {
    val countWhite = tab.toList.flatten.count(x => x == Checkers.Value.white || x == Checkers.Value.whiteKing)
    val countBlack = tab.toList.flatten.count(x => x == Checkers.Value.black || x == Checkers.Value.blackKing)
    (countWhite, countBlack)
  }

  private def isChecker(x: Int, y: Int): Boolean = {
    tab(x)(y) == Checkers.Value.white || tab(x)(y) == Checkers.Value.black
  }


  def isKing(x: Int, y: Int): Boolean = {
    value(x, y) == Checkers.Value.whiteKing || value(x, y) == Checkers.Value.blackKing
  }


  private def value(x: Int, y: Int): Int = {
    tab(x)(y)
  }

  private def ascendIntoKing(x: Int, y: Int): Unit = {
    if(isChecker(x, y))
      if(value(x, y) == Checkers.Value.black)
        tab(x)(y) = Checkers.Value.blackKing
      else
        tab(x)(y) = Checkers.Value.whiteKing
  }

  private def checkColor(x: Int, y: Int): Int = {
    if (value(x, y) == Checkers.Value.white || value(x, y) == Checkers.Value.whiteKing)
      Checkers.Value.white
    else if (value(x, y) == Checkers.Value.black || value(x, y) == Checkers.Value.blackKing)
      Checkers.Value.black
    else
      Checkers.Value.empty
  }

  private def direction(x_s: Int, y_s: Int, x_e: Int, y_e: Int, v: Int): Int = {
    if (x_e + v == x_s && y_e - v == y_s)
      0//left up
    else if (x_e - v == x_s && y_e - v == y_s)
      1//right up
    else if (x_e - v == x_s && y_e + v == y_s)
      2//right down
    else
      3//left down
  }


  def onTheWay(x: Int, y: Int, cx: Int, cy: Int): Int = {
    if (value(cx, cy) == Checkers.Value.empty) {
      0
    }
    else if ((value(x,y) == Checkers.Value.blackKing && (value(cx,cy) == Checkers.Value.whiteKing || value(cx,cy) == Checkers.Value.white))||
      value(x,y) == Checkers.Value.whiteKing && (value(cx,cy) == Checkers.Value.blackKing || value(cx,cy) == Checkers.Value.black)) {
      1
    }
    else {
      2
    }
  }

  private def moveChecker(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (canMove(x_s, y_s, x_e, y_e) && math.abs(y_e - y_s) == 1 && canMoveForward(x_s, y_s, x_e, y_e)) {
      val value = tab(x_s)(y_s)
      tab(x_e)(y_e) = value
      tab(x_s)(y_s) = Checkers.Value.empty
    }
  }

  private def canMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if ((x_s == x_e || y_s == y_e) ||
      value(x_e, y_e) != Checkers.Value.empty || math.abs(y_s - y_e) != math.abs(x_s - x_e))
      false
    else
      true
  }

  private def canMoveForward(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (canMove(x_s, y_s, x_e, y_e) && value(x_s, y_s) == Checkers.Value.white && x_e < x_s && (y_e < y_s || y_e > y_s))
      true
    else if (canMove(x_s, y_s, x_e, y_e) && value(x_s, y_s) == Checkers.Value.black && x_e > x_s && (y_e < y_s || y_e > y_s))
      true
    else
      false
  }

  private def canStrikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s) && canMove(x_s, y_s, x_e, y_e) && value((x_s + x_e) / 2, (y_s + y_e) / 2) * value(x_s, y_s) < 0) {
      true
    }
    else
      false
  }

  private def strikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (canStrikeOnce(x_s, y_s, x_e, y_e)) {
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = Checkers.Value.empty
      tab((x_s + x_e) / 2)((y_s + y_e) / 2) = Checkers.Value.empty

    }
  }

  private def findAllStrikePaths(color_ : Int): List[Movement] = {
    val color = color_
    val movements = for {
      i <- 0 until 8
      j <- 0 until 8
      if j != i
    } yield {
      if (value(i , j) == color)
        findStrikePath(i, j, new Movement(List[(Int, Int, Int, Int)]()), this)
      else
        List[Movement]()
    }
    movements.toList.flatten
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
        && x + 1 <= 7 && y + 1 <= 7
        && canMoveForward(x, y, x + 1, y + 1)) {
        new Movement(List((x, y, x + 1, y + 1)))
      }
      else
        new Movement(List()),
      if (isChecker(x, y)
        && x + 1 <= 7 && y - 1 >= 0
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
    (movements.toList.flatten.flatten ++ findAllKingMovePaths(color)).filter(_.move.nonEmpty)
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
      (b.value(x - 1, y + 1) * v < 0 && b.value(x - 2, y + 2) == Checkers.Value.empty)) {
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
      (b.value(x + 1, y + 1)* v < 0 && b.value(x + 2, y + 2) == Checkers.Value.empty)) {
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
      (b.value(x + 1, y - 1)* v < 0 && b.value(x + 2, y - 2) == Checkers.Value.empty)) {
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
      (b.value(x - 1, y - 1) * v < 0 && b.value(x - 2, y - 2) == Checkers.Value.empty)) {
      val board = copyBoard(b)
      board.move(x, y, x - 2, y - 2)
      findStrikePath(x - 2, y - 2, new Movement(m.move :+ (x, y, x - 2, y - 2)), board)
    }
    else{
      List[Movement]()
    }
  }

  def moveKing(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isKing(x_s, y_s) && canKingMove(x_s, y_s, x_e, y_e)){
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = Checkers.Value.empty
    }
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
        onTheWay(x_s, y_s, x_s - i, y_s + i) //right up
      else if (dir == 1)
        onTheWay(x_s, y_s, x_s + i, y_s + i) //right down
      else if (dir == 2)
        onTheWay(x_s, y_s, x_s + i, y_s - i) //left down
      else
        onTheWay(x_s, y_s, x_s - i, y_s - i) //left up
    }
    if (res.sum == 0) {
      true
    }
    else
      false
  }

  def findAllKingStrikePaths(c: Int): List[Movement] = {
    val movements = for {
      i <- 0 until 8
    } yield {
      for {
        j <- 0 until 8
        if j != i
      } yield {
        if (isKing(i, j) && checkColor(i, j) == c) {
          findStrikePathKing(i, j, new Movement(List[(Int, Int, Int, Int)]()), this)
        }
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
      if (isKing(i, j) && checkColor(i, j) == c) {
        //println("findAllKingMovePaths")
        findMovePathKing(i, j)
      }
      else
        List[Movement]()
    }
    movements.toList.flatten
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
    val l = findAllStrikePaths(s) ++ possibleKingStrikePaths(s)
    val lengthList = l.map(_.move.length)
    if (lengthList.nonEmpty) {
      val maxLength = lengthList.max
      l.filter(_.move.length == maxLength)
    }
    else {
      List[Movement]()
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

  def findMovePathKing(x: Int, y: Int): List[Movement] = {
    List[Movement]() ++ findMovePathKingUpRight(x, y) ++ findMovePathKingDownRight(x, y) ++ findMovePathKingDownLeft(x, y) ++ findMovePathKingUpLeft(x, y)
  }

  def findMovePathKingUpRight(x: Int, y: Int): List[Movement] = {
    val list = for {
      i <- 1 until 8
    } yield {
      if (x - i >= 0 && y + i <= 7 && canKingMove(x, y, x - i, y + i)) {
        new Movement(List((x, y, x - i, y + i)))
      }
      else
        new Movement(List())
    }
    list.toList.filter(_.move.nonEmpty)
  }

  def findMovePathKingDownRight(x: Int, y: Int): List[Movement] = {
    val list = for {
      i <- 1 until 8
    } yield {
      if (x + i <= 7 && y + i <= 7 && canKingMove(x, y, x + i, y + i)) {
        new Movement(List((x, y, x + i, y + i)))
      }
      else
        new Movement(List())
    }
    list.toList.filter(_.move.nonEmpty)
  }

  def findMovePathKingDownLeft(x: Int, y: Int): List[Movement] = {
    val list = for {
      i <- 1 until 8
    } yield {
      if (x + i <= 7 && y - i >= 0 && canKingMove(x, y, x + i, y - i)) {
        new Movement(List((x, y, x + i, y - i)))
      }
      else
        new Movement(List())
    }
    list.toList.filter(_.move.nonEmpty)
  }

  def findMovePathKingUpLeft(x: Int, y: Int): List[Movement] = {
    val list = for {
      i <- 1 until 8
    } yield {
      if (x - i >= 0 && y - i >= 0 && canKingMove(x, y, x - i, y - i)) {
        new Movement(List((x, y, x - i, y - i)))
      }
      else
        new Movement(List())
    }
    list.toList.filter(_.move.nonEmpty)
  }

  def findStrikePathKing(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    if (b.value(x,y)==0)
      return List[Movement](m)

    val list = List[Movement](m) ++ findStrikeKingLeftDown(x,y,m,b) ++ findStrikeKingRightDown(x,y,m,b) ++ findStrikeKingRightUp(x,y,m,b) ++ findStrikeKingLeftUp(x,y,m,b)
    list.filter(_.move.nonEmpty)
  }

  def findStrikeKingRightUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8
      if x - i >= 0 && y + i <= 7
    } yield {
      if (b.canKingStrike(x, y, x - i, y + i)) {
        val board = copyBoard(b)
        board.move(x, y, x - i, y + i)
        findStrikePathKing(x - i, y + i, new Movement(m.move :+ (x, y, x - i, y + i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findStrikeKingRightDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8
      if x + i <= 7 && y + i <= 7
    } yield {
      if (b.canKingStrike(x, y, x + i, y + i)) {
        val board = copyBoard(b)
        board.move(x, y, x + i, y + i)
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
      i <- 1 until 8
      if x + i <= 7 && y - i >= 0
      if b.canKingStrike(x, y, x + i, y - i)
    } yield {
      if (b.canKingStrike(x, y, x + i, y - i)) {
        val board = copyBoard(b)
        board.move(x, y, x + i, y - i)
        findStrikePathKing(x + i, y - i, new Movement(m.move :+ (x, y, x + i, y - i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }

  def findStrikeKingLeftUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    val res = for {
      i <- 1 until 8
      if x - i >= 0 && y - i >= 0
    } yield {
      if (b.canKingStrike(x, y, x - i, y - i)) {
        val board = copyBoard(b)
        board.move(x, y, x - i, y - i)
        findStrikePathKing(x - i, y - i, new Movement(m.move :+ (x, y, x - i, y - i)), board)
      }
      else
        List[Movement]()
    }
    res.toList.flatten
  }


  def kingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    val inc = math.abs(y_e - y_s)
    val dir = direction(x_s, y_s, x_e, y_e, inc)
    if (isKing(x_s, y_s) && canKingStrike(x_s, y_s, x_e, y_e)) {
      for {
        i <- 1 until inc
      } yield {
        if (dir == 0)
          tab(x_s - i)(y_s + i) = 0 //right up
        else if (dir == 1)
          tab(x_s + i)(y_s + i) = 0 //right down
        else if (dir == 2)
          tab(x_s + i)(y_s - i) = 0 //left down
        else
          tab(x_s - i)(y_s - i) = 0//left up
      }
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = Checkers.Value.empty
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
    if(canBecomeKing(m.move(len - 1)._3, m.move(len - 1)._4))
      ascendIntoKing(m.move(len - 1)._3, m.move(len - 1)._4)
  }

  def canBecomeKing(x: Int, y: Int): Boolean = {
    isChecker(x, y) && (checkColor(x, y) == Checkers.Value.white && x == 0) || (checkColor(x, y) == Checkers.Value.black && x == 7)
  }


}