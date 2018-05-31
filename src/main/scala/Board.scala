/**
  * Class that contains Array that portraits actual board state and function to change and operate on it.
  * @param t Initial array
  */
class Board(t: Array[Array[Int]]){
  val tab:Array[Array[Int]] = t

  /**
    * Function to copy board to other instance
    * @param b - board to copy from
    * @return new copied board
    */
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

  /**
    * Function to change from value in array to printed value of position in console
    * @param x - value of x coordinate
    * @param y - value of y coordinate
    * @return - printed value
    */
  private def printing(x: Int, y: Int): Any = {
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

  /**
    * Function that uses function printing to print the whole board
    * @param color - defines in which direction the board will be printed
    */
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

  /**
    * Function to see current value of board
    * @return value of whole board
    */
  def objectiveFunction:Int = {
    tab.map(_.toList).toList.flatten.sum
  }

  /**
    * Function that counts checkers
    * @return counted checkers of each color
    */
  def countCheckers: (Int,Int) = {
    val countWhite = tab.toList.flatten.count(x => x == Checkers.Value.white || x == Checkers.Value.whiteKing)
    val countBlack = tab.toList.flatten.count(x => x == Checkers.Value.black || x == Checkers.Value.blackKing)
    (countWhite, countBlack)
  }

  /**
    * Function that checks if the current position is occupied by a checker
    * @param x - value of x coordinate
    * @param y - value of y coordinate
    * @return true or false
    */
  private def isChecker(x: Int, y: Int): Boolean = {
    tab(x)(y) == Checkers.Value.white || tab(x)(y) == Checkers.Value.black
  }

  /**
    * Function that checks if the current position is occupied by a king
    * @param x - value of x coordinate
    * @param y - value of y coordinate
    * @return true or false
    */
  private def isKing(x: Int, y: Int): Boolean = {
    value(x, y) == Checkers.Value.whiteKing || value(x, y) == Checkers.Value.blackKing
  }

  /**
    * Function that returns value of position
    * @param x - value of x coordinate
    * @param y - value of y coordinate
    * @return value of position
    */
  private def value(x: Int, y: Int): Int = {
    tab(x)(y)
  }

  /**
    * Function which changes a checker to king
    * @param x - value of x coordinate
    * @param y = value of y coordinate
    */
  private def ascendIntoKing(x: Int, y: Int): Unit = {
    if(isChecker(x, y))
      if(value(x, y) == Checkers.Value.black)
        tab(x)(y) = Checkers.Value.blackKing
      else
        tab(x)(y) = Checkers.Value.whiteKing
  }

  /**
    * Function which checks color of position
    * @param x - value of x coordinate
    * @param y - value of y coordinate
    * @return int that corresponds to color
    */
  private def checkColor(x: Int, y: Int): Int = {
    if (value(x, y) == Checkers.Value.white || value(x, y) == Checkers.Value.whiteKing)
      Checkers.Value.white
    else if (value(x, y) == Checkers.Value.black || value(x, y) == Checkers.Value.blackKing)
      Checkers.Value.black
    else
      Checkers.Value.empty
  }

  /**
    * Function which returns direction where checker or king is heading
    * @param x_s - starting value of x coordinate
    * @param y_s - starting value of y coordinate
    * @param x_e - ending value of x coordinate
    * @param y_e - ending value of y coordinate
    * @param v - value of incrementing
    * @return int corresponding to direction
    */
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

  /**
    * Function that checks if something is on the way of king
    * @param x - starting x coordinate of king
    * @param y - starting y coordinate of king
    * @param cx - current checking x coordinate of path
    * @param cy - current checking y coordinate of path
    * @return - 0 nothing is on the way, 1 - only one opponent is on the way, 2 - ally is on the way
    */
  private def onTheWay(x: Int, y: Int, cx: Int, cy: Int): Int = {
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

  /**
    * Function that moves checker on the board
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    */
  private def moveChecker(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (canMove(x_s, y_s, x_e, y_e) && math.abs(y_e - y_s) == 1 && canMoveForward(x_s, y_s, x_e, y_e)) {
      val value = tab(x_s)(y_s)
      tab(x_e)(y_e) = value
      tab(x_s)(y_s) = Checkers.Value.empty
    }
  }

  /**
    * Function that checks if checker or king can move on to ending position
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    * @return - true or false
    */
  private def canMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    !(x_s == x_e || y_s == y_e || value(x_e, y_e) != Checkers.Value.empty || math.abs(y_s - y_e) != math.abs(x_s - x_e))
  }

  /**
    * Function that checks if checker can move forward
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    * @return true or false
    */
  private def canMoveForward(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (canMove(x_s, y_s, x_e, y_e) && value(x_s, y_s) == Checkers.Value.white && x_e < x_s && (y_e < y_s || y_e > y_s))
      true
    else if (canMove(x_s, y_s, x_e, y_e) && value(x_s, y_s) == Checkers.Value.black && x_e > x_s && (y_e < y_s || y_e > y_s))
      true
    else
      false
  }

  /**
    * Function that checks if checker can strike once
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - x coordinate of place after strike
    * @param y_e - y coordinate of place after strike
    * @return true or false
    */
  private def canStrikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s) && canMove(x_s, y_s, x_e, y_e) && value((x_s + x_e) / 2, (y_s + y_e) / 2) * value(x_s, y_s) < 0) {
      true
    }
    else
      false
  }

  /**
    * Function that strikes for checkers
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - x coordinate of strike
    * @param y_e - y coordinate of strike
    */
  private def strikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (canStrikeOnce(x_s, y_s, x_e, y_e)) {
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = Checkers.Value.empty
      tab((x_s + x_e) / 2)((y_s + y_e) / 2) = Checkers.Value.empty
    }
  }

  /**
    * Function that finds all strike paths for chosen color
    * @param color_ color to check strike paths for
    * @return List of Movements
    */
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

  /**
    * Function that finds move paths for chosen position
    * @param x - x coordinate
    * @param y - y coordinate
    * @return - List of Movements if any exists
    */
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

  /**
    * Function that finds all move paths for checkers and kings of chosen color
    * @param color_ - chosen color
    * @return - List of Movements for chosen color
    */
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

  /**
    * Recursive function to find all strike paths for chosen position
    * @param x - value of current x coordinate
    * @param y - value of current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements that contains strike paths
    */
  private def findStrikePath(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    if(b.value(x,y)==0)
      return List[Movement](m)

    val list = List[Movement](m) ++ findStrikeLeftDown(x,y,m,b) ++ findStrikeRightDown(x,y,m,b) ++ findStrikeRightUp(x,y,m,b) ++ findStrikeLeftUp(x,y,m,b)
    list.filter(_.move.nonEmpty)
  }

  /**
    * Function that checks strikes for Right Up
    * @param x - value of current x coordinate
    * @param y - value of current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements that contains strike paths
    */
  private def findStrikeRightUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
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

  /**
    * Function that checks strikes for Right Down
    * @param x - value of current x coordinate
    * @param y - value of current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements that contains strike paths
    */
  private def findStrikeRightDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
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

  /**
    * Function that checks strikes for Left Down
    * @param x - value of current x coordinate
    * @param y - value of current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements that contains strike paths
    */
  private def findStrikeLeftDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
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

  /**
    * Function that checks strikes for Left Up
    * @param x - value of current x coordinate
    * @param y - value of current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements that contains strike paths
    */
  private def findStrikeLeftUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
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

  /**
    * Function that moves King
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    */
  private def moveKing(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isKing(x_s, y_s) && canKingMove(x_s, y_s, x_e, y_e)){
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = Checkers.Value.empty
    }
  }

  /**
    * Function that checks if king can move to ending position
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    * @return true or false
    */
  private def canKingMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
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

  /**
    * Function that finds all kings strike paths for chosen color
    * @param c - chosen color
    * @return - List of strike paths for kings in chosen color (if any exists)
    */
  private def findAllKingStrikePaths(c: Int): List[Movement] = {
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

  /**
    * Function that finds all king move paths for chosen color
    * @param c - chosen color
    * @return - List of movements for kings of chosen color (if any exists)
    */
  private def findAllKingMovePaths(c : Int): List[Movement] = {
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

  /**
    * Function that returns all possible move paths for kings of chosen color
    * @param c - chosen color
    * @return - List of possible Movements for kings of chosen color
    */
  private def possibleKingMovePaths(c: Int): List[Movement] = {
    val l = findAllKingMovePaths(c)
    val lengthList = l.map(_.move.length)
    if (lengthList.nonEmpty) {
      val maxLength = lengthList.max
      l.filter(_.move.length == maxLength)
    }
    else
      List[Movement]()
  }

  /**
    * Function that returns only the longest strike path for kings of the chosen color
    * @param c - chosen color
    * @return - List of Movement
    */
  private def possibleKingStrikePaths(c: Int): List[Movement] = {
    val l = findAllKingStrikePaths(c)
    val lengthList = l.map(_.move.length)
    if (lengthList.nonEmpty) {
      val maxLength = lengthList.max
      l.filter(_.move.length == maxLength)
    }
    else
      List[Movement]()
  }

  /**
    * Function that returns the longest (value wise) strike path from among checkers and kings of chosen color
    * @param s - chosen color
    * @return - List of Movement (if any)
    */
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

  /**
    * Function which checks if king can move to ending position
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    * @return - true or false
    */
  private def canKingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
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

  /**
    * Function that returns all king move paths for chosen position
    * @param x - x coordintae
    * @param y - y coordinate
    * @return - List of Movements (if any exists)
    */
  private def findMovePathKing(x: Int, y: Int): List[Movement] = {
    List[Movement]() ++ findMovePathKingRightUp(x, y) ++ findMovePathKingRightDown(x, y) ++ findMovePathKingLeftDown(x, y) ++ findMovePathKingLeftUp(x, y)
  }

  /**
    * Function that checks for king move paths Right Up
    * @param x - x coordinate
    * @param y - y coordinate
    * @return - List of Movements (if any exists)
    */
  private def findMovePathKingRightUp(x: Int, y: Int): List[Movement] = {
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

  /**
    * Function that checks for king move paths Right Down
    * @param x - x coordinate
    * @param y - y coordinate
    * @return - List of Movements (if any exists)
    */
  private def findMovePathKingRightDown(x: Int, y: Int): List[Movement] = {
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

  /**
    * Function that checks for king move paths Left Down
    * @param x - x coordinate
    * @param y - y coordinate
    * @return - List of Movements (if any exists)
    */
  private def findMovePathKingLeftDown(x: Int, y: Int): List[Movement] = {
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

  /**
    * Function that checks for king move paths Left Up
    * @param x - x coordinate
    * @param y - y coordinate
    * @return - List of Movements (if any exists)
    */
  private def findMovePathKingLeftUp(x: Int, y: Int): List[Movement] = {
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

  /**
    * Recursive function that finds all strike paths for king in chosen position
    * @param x - current x coordinate
    * @param y - current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements (if any exists)
    */
  private def findStrikePathKing(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    if (b.value(x,y)==0)
      return List[Movement](m)

    val list = List[Movement](m) ++ findStrikeKingLeftDown(x,y,m,b) ++ findStrikeKingRightDown(x,y,m,b) ++ findStrikeKingRightUp(x,y,m,b) ++ findStrikeKingLeftUp(x,y,m,b)
    list.filter(_.move.nonEmpty)
  }

  /**
    * Function that checks for king strike paths Right Up
    * @param x - current x coordinate
    * @param y - current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements (if any exists)
    */
  private def findStrikeKingRightUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
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

  /**
    * Function that checks for king strike paths Right Down
    * @param x - current x coordinate
    * @param y - current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements (if any exists)
    */
  private def findStrikeKingRightDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
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

  /**
    * Function that checks for king strike paths Left Down
    * @param x - current x coordinate
    * @param y - current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements (if any exists)
    */
  private def findStrikeKingLeftDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
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

  /**
    * Function that checks for king strike paths Left Up
    * @param x - current x coordinate
    * @param y - current y coordinate
    * @param m - current movement
    * @param b - current board
    * @return - List of Movements (if any exists)
    */
  private def findStrikeKingLeftUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
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

  /**
    * Function to execute king strike
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    */
  private def kingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
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

  /**
    * Function that moves checkers and kings on board
    * @param x_s - starting x coordinate
    * @param y_s - starting y coordinate
    * @param x_e - ending x coordinate
    * @param y_e - ending y coordinate
    * @return - true or false if successful
    */
  private def move(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s))
      if (math.abs(x_s - x_e) == 2) {
        strikeOnce(x_s, y_s, x_e, y_e)
        false
      }
      else {
        moveChecker(x_s, y_s, x_e, y_e)
        false
      }
    else if (isKing(x_s, y_s)) {
      if (canKingStrike(x_s, y_s, x_e, y_e)) {
        kingStrike(x_s, y_s, x_e, y_e)
        false
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

  /**
    * Function that executes movement from exact Movement
    * @param m - movement to do
    */
  def executeMovement(m: Movement): Boolean = {
    val len =  m.move.length
    val kingMoveOnly =
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
    kingMoveOnly.reduce(_ && _)
  }

  /**
    * Function that checks if checker can become king
    * @param x - x coordinate
    * @param y - y coordinate
    * @return - true or false
    */
  private def canBecomeKing(x: Int, y: Int): Boolean = {
    isChecker(x, y) && (checkColor(x, y) == Checkers.Value.white && x == 0) || (checkColor(x, y) == Checkers.Value.black && x == 7)
  }


}