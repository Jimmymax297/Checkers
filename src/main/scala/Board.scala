class Board {
  val white = -1
  val empty = 0
  val black = 1
  val whiteKing = -2
  val blackKing = 2
  val tab:Array[Array[Int]] = {
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
  }

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

  def color(x: Int, y: Int): Any = {
    if (value(x, y) == empty)
      "board"
    else {
      if (value(x, y) == white || value(x, y) == whiteKing)
        "white"
      else
        "black"
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

  def canStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s) && canMove(x_s, y_s, x_e, y_e) && value((x_s + x_e) / 2, (y_s + y_e) / 2) == -value(x_s, y_s)) {
      println("canStrike")
      true
    }
    else
      false
  }

  def strikeOnce(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (canStrike(x_s, y_s, x_e, y_e)) {
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = empty
      tab((x_s + x_e) / 2)((y_s + y_e) / 2) = empty
    }
  }

  def canStrikeAgain(x_s: Int, y_s: Int): Boolean = {
    val v = value(x_s, y_s)
    if ((value(x_s - 1, y_s + 1) == -v && value(x_s - 2, y_s + 2) == empty) ||
        (value(x_s + 1, y_s + 1) == -v && value(x_s + 2, y_s + 2) == empty) ||
        (value(x_s + 1, y_s - 1) == -v && value(x_s + 2, y_s - 2) == empty) ||
        (value(x_s - 1, y_s - 1) == -v && value(x_s - 2, y_s - 2) == empty))
      true
    else
      false
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
    if (!canMove(x_s, y_s, x_e, y_e))
      return false
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
    println(res.sum)
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

  def move(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isChecker(x_s, y_s))
      if (math.abs(x_s - x_e) == 2)
        strikeOnce(x_s, y_s, x_e, y_e)
      else
        moveChecker(x_s, y_s, x_e, y_e)
    else if (isKing(x_s, y_s))
      if (canKingStrike(x_s, y_s, x_e, y_e))
        kingStrike(x_s, y_s, x_e, y_e)
      else
        moveKing(x_s, y_s, x_e, y_e)
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