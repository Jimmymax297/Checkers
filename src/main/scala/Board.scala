import java.lang.Cloneable

class Board(t: Array[Array[Int]]) extends Cloneable{
  val white: Int = -1
  val empty: Int = 0
  val black: Int = 1
  val whiteKing: Int = -2
  val blackKing: Int = 2
  val tab:Array[Array[Int]] = t
  /*{
    val t = Array(
      Array(empty, black, empty, black, empty, black, empty, black),
      Array(black, empty, black, empty, black, empty, black, empty),
      Array(empty, black, empty, black, empty, black, empty, black),
      Array(empty, empty, empty, empty, empty, empty, empty, empty),
      Array(empty, empty, empty, empty, empty, empty, empty, empty),
      Array(white, empty, white, empty, white, empty, white, empty),
      Array(empty, white, empty, white, empty, white, empty, white),
      Array(white, empty, white, empty, white, empty, white, empty))
    t
  }*/

  /*(def this(other: Board) {
    this
    val this.white = -1
    val this.empty = 0
    val this.black = 1
    val this.whiteKing = -2
    val this.blackKing = 2
    val this.tab = Array(
  Array(empty, other.tab(0)(1), empty, other.tab(0)(3), empty, other.tab(0)(5), empty, other.tab(0)(7)),
  Array(other.tab(1)(0), empty, other.tab(1)(2), empty, other.tab(1)(4), empty, other.tab(1)(6), empty),
  Array(empty, other.tab(2)(1), empty, other.tab(2)(3), empty, other.tab(2)(5), empty, other.tab(2)(7)),
  Array(other.tab(3)(0), empty, other.tab(3)(2), empty, other.tab(3)(4), empty, other.tab(3)(6), empty),
  Array(empty, other.tab(4)(1), empty, other.tab(4)(3), empty, other.tab(4)(5), empty, other.tab(4)(7)),
  Array(other.tab(5)(0), empty, other.tab(5)(2), empty, other.tab(5)(4), empty, other.tab(5)(6), empty),
  Array(empty, other.tab(6)(1), empty, other.tab(6)(3), empty, other.tab(6)(5), empty, other.tab(6)(7)),
  Array(other.tab(7)(0), empty, other.tab(7)(2), empty, other.tab(7)(4), empty, other.tab(7)(6), empty))
  }*/

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
    println(math.abs(y_e - y_s))
    if (canMove(x_s, y_s, x_e, y_e) && math.abs(y_e - y_s) == 1) {
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

  def canStrikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s) && canMove(x_s, y_s, x_e, y_e) && value((x_s + x_e) / 2, (y_s + y_e) / 2) == -value(x_s, y_s)) {
      //println("canStrike")
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
    /*if(x_s - 1 >=0 && y_s + 1 <= 7 && x_s - 2 >= 0 && y_s + 2 <= 7)//lewo dol
      value(x_s - 1, y_s + 1) == -v && value(x_s - 2, y_s + 2) == empty
    if(x_s + 1 <= 7 && y_s + 1 <= 7 && x_s + 2 <= 7 && y_s + 2 <= 7)//prawo dol
      value(x_s + 1, y_s + 1) == -v && value(x_s + 2, y_s + 2) == empty
    if(x_s + 1 <= 7 && y_s - 1 >= 0 && x_s + 2 <= 7 && y_s - 2 >= 0)//prawo gora
      value(x_s + 1, y_s - 1) == -v && value(x_s + 2, y_s - 2) == empty
    if(x_s - 1 >= 0 && y_s - 1 >= 0 && x_s - 2 >= 7 && y_s - 2 >= 0)//lewo gora
      value(x_s - 1, y_s - 1) == -v && value(x_s - 2, y_s - 2) == empty
    false*/
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

  def findStrikePath(x: Int, y: Int, m: Movement, b: Board): List[Movement] = {
    /*val v = value(x, y)

    if ((x - 1 >=0 && y + 1 <= 7 && x - 2 >= 0 && y + 2 <= 7)&&
      (value(x - 1, y + 1) == -v && value(x - 2, y + 2) == empty)) {
      findStrikePath(x - 2, y + 2, new Movement(m.move :+ (x, y, x - 2, y + 2)))
    }
    if ((x + 1 <= 7 && y + 1 <= 7 && x + 2 <= 7 && y + 2 <= 7)&&
      (value(x + 1, y + 1) == -v && value(x + 2, y + 2) == empty)) {
      findStrikePath(x + 2, y + 2, new Movement(m.move :+ (x, y, x + 2, y + 2)))
    }
    if ((x + 1 <= 7 && y - 1 >= 0 && x + 2 <= 7 && y - 2 >= 0)&&
      (value(x + 1, y - 1) == -v && value(x + 2, y - 2) == empty)) {
      findStrikePath(x + 2, y - 2, new Movement(m.move :+ (x, y, x + 2, y - 2)))
    }
    if ((x - 1 >= 0 && y - 1 >= 0 && x - 2 >= 0 && y - 2 >= 0)&&
      (value(x - 1, y - 1) == -v && value(x - 2, y - 2) == empty)) {
      findStrikePath(x - 2, y - 2, new Movement(m.move :+ (x, y, x - 2, y - 2)))
    }*/
    if(b.value(x,y)==0)
      return List[Movement]()

    b.drawBoard()
    println("findStrikePath")
    val listOfLists :List[List[Movement]] = List (findStrikeLeftDown(x,y,m,b),findStrikeRightDown(x,y,m,b),findStrikeRightUp(x,y,m,b),findStrikeLeftUp(x,y,m,b))
    return listOfLists/*.filter(_.nonEmpty)*/.flatten.filter(_.move.nonEmpty)
  }

  def findStrikeLeftDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    println("findStrikeLeftDown")
    print(x)
    print(" ")
    print(y)
    print("\n")
    b.drawBoard()
    if ((x - 1 >=0 && y + 1 <= 7 && x - 2 >= 0 && y + 2 <= 7)&&
      (b.value(x - 1, y + 1) == -v && b.value(x - 2, y + 2) == empty)) {
      println("IN")
      println("color: ",v,"enemy: ",b.value(x-1,y+1),"target: ",b.value(x-2,y+2))
      //val board = new Board(b)
      val newTab = Array(
        Array(empty, b.tab(0)(1), empty, b.tab(0)(3), empty, b.tab(0)(5), empty, b.tab(0)(7)),
        Array(b.tab(1)(0), empty, b.tab(1)(2), empty, b.tab(1)(4), empty, b.tab(1)(6), empty),
        Array(empty, b.tab(2)(1), empty, b.tab(2)(3), empty, b.tab(2)(5), empty, b.tab(2)(7)),
        Array(b.tab(3)(0), empty, b.tab(3)(2), empty, b.tab(3)(4), empty, b.tab(3)(6), empty),
        Array(empty, b.tab(4)(1), empty, b.tab(4)(3), empty, b.tab(4)(5), empty, b.tab(4)(7)),
        Array(b.tab(5)(0), empty, b.tab(5)(2), empty, b.tab(5)(4), empty, b.tab(5)(6), empty),
        Array(empty, b.tab(6)(1), empty, b.tab(6)(3), empty, b.tab(6)(5), empty, b.tab(6)(7)),
        Array(b.tab(7)(0), empty, b.tab(7)(2), empty, b.tab(7)(4), empty, b.tab(7)(6), empty))
      val board = new Board(newTab)
      //val board = b.clone.asInstanceOf[Board]
      board.move(x, y, x - 2, y + 2)
      findStrikePath(x - 2, y + 2, new Movement(m.move :+ (x, y, x - 2, y + 2)), board)
    }
    else {
      println("OUT")
      List[Movement](m)
    }
  }

  def findStrikeRightDown(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    println("findStrikeRightDown")
    print(x)
    print(" ")
    print(y)
    print("\n")
    b.drawBoard()
    if ((x + 1 <= 7 && y + 1 <= 7 && x + 2 <= 7 && y + 2 <= 7)&&
      (b.value(x + 1, y + 1) == -v && b.value(x + 2, y + 2) == empty)) {
      println("IN")
      println("color: ",v,"enemy: ",b.value(x+1,y+1),"target: ",b.value(x+2,y+2))
      //val board = new Board(b)
      //val board = b.clone.asInstanceOf[Board]
      val newTab = Array(
        Array(empty, b.tab(0)(1), empty, b.tab(0)(3), empty, b.tab(0)(5), empty, b.tab(0)(7)),
        Array(b.tab(1)(0), empty, b.tab(1)(2), empty, b.tab(1)(4), empty, b.tab(1)(6), empty),
        Array(empty, b.tab(2)(1), empty, b.tab(2)(3), empty, b.tab(2)(5), empty, b.tab(2)(7)),
        Array(b.tab(3)(0), empty, b.tab(3)(2), empty, b.tab(3)(4), empty, b.tab(3)(6), empty),
        Array(empty, b.tab(4)(1), empty, b.tab(4)(3), empty, b.tab(4)(5), empty, b.tab(4)(7)),
        Array(b.tab(5)(0), empty, b.tab(5)(2), empty, b.tab(5)(4), empty, b.tab(5)(6), empty),
        Array(empty, b.tab(6)(1), empty, b.tab(6)(3), empty, b.tab(6)(5), empty, b.tab(6)(7)),
        Array(b.tab(7)(0), empty, b.tab(7)(2), empty, b.tab(7)(4), empty, b.tab(7)(6), empty))
      val board = new Board(newTab)
      board.move(x, y, x + 2, y + 2)
      findStrikePath(x + 2, y + 2, new Movement(m.move :+ (x, y, x + 2, y + 2)), board)
    }
    else
      List[Movement](m)
  }

  def findStrikeRightUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    println("findStrikeRightUp")
    print(x)
    print(" ")
    print(y)
    print("\n")
    b.drawBoard()
    if ((x + 1 <= 7 && y - 1 >= 0 && x + 2 <= 7 && y - 2 >= 0)&&
      (b.value(x + 1, y - 1) == -v && b.value(x + 2, y - 2) == empty)) {
      println("IN")
      println("color: ",v,"enemy: ",b.value(x+1,y-1),"target: ",b.value(x+2,y-2))
      //val board = new Board(b)
      //val board = b.clone.asInstanceOf[Board]
      val newTab = Array(
        Array(empty, b.tab(0)(1), empty, b.tab(0)(3), empty, b.tab(0)(5), empty, b.tab(0)(7)),
        Array(b.tab(1)(0), empty, b.tab(1)(2), empty, b.tab(1)(4), empty, b.tab(1)(6), empty),
        Array(empty, b.tab(2)(1), empty, b.tab(2)(3), empty, b.tab(2)(5), empty, b.tab(2)(7)),
        Array(b.tab(3)(0), empty, b.tab(3)(2), empty, b.tab(3)(4), empty, b.tab(3)(6), empty),
        Array(empty, b.tab(4)(1), empty, b.tab(4)(3), empty, b.tab(4)(5), empty, b.tab(4)(7)),
        Array(b.tab(5)(0), empty, b.tab(5)(2), empty, b.tab(5)(4), empty, b.tab(5)(6), empty),
        Array(empty, b.tab(6)(1), empty, b.tab(6)(3), empty, b.tab(6)(5), empty, b.tab(6)(7)),
        Array(b.tab(7)(0), empty, b.tab(7)(2), empty, b.tab(7)(4), empty, b.tab(7)(6), empty))
      val board = new Board(newTab)
      board.move(x, y, x + 2, y - 2)
      findStrikePath(x + 2, y - 2, new Movement(m.move :+ (x, y, x + 2, y - 2)), board)
    }
    else
      List[Movement](m)
  }

  def findStrikeLeftUp(x: Int, y: Int, m: Movement, b: Board): List[Movement] ={
    val v = b.value(x, y)
    println("findStrikeLeftUp")
    println(x, y)
    b.drawBoard()
    if ((x - 1 >= 0 && y - 1 >= 0 && x - 2 >= 0 && y - 2 >= 0)&&
      (b.value(x - 1, y - 1) == -v && b.value(x - 2, y - 2) == empty)) {
      println("IN")
      println("color: ",v,"enemy: ",b.value(x-1,y-1),"target: ",b.value(x-2,y-2))
      //val board = new Board(b)
      //val board = b.clone.asInstanceOf[Board]
      val newTab = Array(
        Array(empty, b.tab(0)(1), empty, b.tab(0)(3), empty, b.tab(0)(5), empty, b.tab(0)(7)),
        Array(b.tab(1)(0), empty, b.tab(1)(2), empty, b.tab(1)(4), empty, b.tab(1)(6), empty),
        Array(empty, b.tab(2)(1), empty, b.tab(2)(3), empty, b.tab(2)(5), empty, b.tab(2)(7)),
        Array(b.tab(3)(0), empty, b.tab(3)(2), empty, b.tab(3)(4), empty, b.tab(3)(6), empty),
        Array(empty, b.tab(4)(1), empty, b.tab(4)(3), empty, b.tab(4)(5), empty, b.tab(4)(7)),
        Array(b.tab(5)(0), empty, b.tab(5)(2), empty, b.tab(5)(4), empty, b.tab(5)(6), empty),
        Array(empty, b.tab(6)(1), empty, b.tab(6)(3), empty, b.tab(6)(5), empty, b.tab(6)(7)),
        Array(b.tab(7)(0), empty, b.tab(7)(2), empty, b.tab(7)(4), empty, b.tab(7)(6), empty))
      val board = new Board(newTab)
      board.move(x, y, x - 2, y - 2)
      findStrikePath(x - 2, y - 2, new Movement(m.move :+ (x, y, x - 2, y - 2)), board)
    }
    else
      List[Movement](m)
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
        onTheWay(inc, x_s, y_s, x_s - i, y_s + i)
      else if (dir == 1)
        onTheWay(inc, x_s, y_s, x_s + i, y_s + i)
      else if (dir == 2)
        onTheWay(inc, x_s, y_s, x_s + i, y_s - i)
      else
        onTheWay(inc, x_s, y_s, x_s - i, y_s - i)
    }
    if (res.sum == 0) {
      println("king can move")
      true
    }
    else
      false
  }

  def onTheWay(inc: Int, x: Int, y: Int, cx: Int, cy: Int): Int = {
    println(math.abs(cx - x))
    if (value(cx, cy) == empty) {
      println("Adding 0")
      0
    }
    else if ((value(x,y) == blackKing && (value(cx,cy) == whiteKing || value(cx,cy) == white))||
      value(x,y) == whiteKing && (value(cx,cy) == blackKing || value(cx,cy) == black)) {

      println("Adding 1")
      1
    }
    else {
      println("Adding 2")
      2
    }
  }

  def canKingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (!canMove(x_s, y_s, x_e, y_e)) {
      println("Can't move")
      return false
    }
    val inc = math.abs(y_e - y_s)
    val dir = direction(x_s, y_s, x_e, y_e, inc)
    val res = for{
      i <- 1 until inc
      if math.abs(y_e - y_s) == math.abs(x_e - x_s)
    } yield {
      if (dir == 0)
      onTheWay(inc, x_s, y_s, x_s - i, y_s + i)
      else if (dir == 1)
        onTheWay(inc, x_s, y_s, x_s + i, y_s + i)
      else if (dir == 2)
        onTheWay(inc, x_s, y_s, x_s + i, y_s - i)
      else
        onTheWay(inc, x_s, y_s, x_s - i, y_s - i)
    }
    println(res)
    res.sum == 1
  }

  def moveKing(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isKing(x_s, y_s) && canKingMove(x_s, y_s, x_e, y_e)){
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = empty
    }
  }

  def kingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isKing(x_s, y_s) && canKingStrike(x_s, y_s, x_e, y_e)) {
      for {
        i <- 1 until math.abs(y_e - y_s)
      } yield {
        tab(x_e + i)(y_e + i) = empty
      }
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = empty
    }
  }

  def move(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s))
      if (math.abs(x_s - x_e) == 2) {
        strikeOnce(x_s, y_s, x_e, y_e)
        if (canStrike(x_e, y_e))
          println("You must strike again, do it!")
        true
      }
      else {
        moveChecker(x_s, y_s, x_e, y_e)
        true
      }
    else if (isKing(x_s, y_s)) {
      val strike = canKingStrike(x_s, y_s, x_e, y_e)
      println(strike)
      if (strike) {
        println("King strike!")
        kingStrike(x_s, y_s, x_e, y_e)
        true
      }
      else {
        println("King move!")
        moveKing(x_s, y_s, x_e, y_e)
        true
      }
    }
    else{
      println("can't move ")
      println(x_s, y_s, x_e, y_e)
      false
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
    println("     0    1    2    3    4    5    6    7\n")
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