class Board {
  val tab:Array[Array[Int]] = {
    val t = Array(Array(0, 1, 0, 1, 0, 1, 0, 1),
      Array(1, 0, 1, 0, 1, 0, 1, 0),
      Array(0, 1, 0, 1, 0, 1, 0, 1),
      Array(0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0),
      Array(-1, 0,-1, 0,-1, 0,-1, 0),
      Array(0,-1, 0,-1, 0,-1, 0,-1),
      Array(-1, 0,-1, 0,-1, 0,-1, 0))
    t
  }

  def isChecker(x: Int, y: Int): Boolean = {
    if(tab(x)(y) == 1 || tab(x)(y) == -1)
      true
    else
      false
  }

  def value(x: Int, y: Int): Int = tab(x)(y)

  def change(x: Int, y: Int, value: Int): Unit = tab(x)(y) = value

  def king(x: Int, y: Int): Unit = {
    if(isChecker(x, y))
      if(value(x, y) == 1)
        change(x, y, 2)
      else
        change(x, y, -2)
  }

  def color(x: Int, y: Int): Any = {
    if (value(x, y) == 0)
      "board"
    else {
      if (value(x, y) > 0)
        "white"
      else if (value(x, y) < 0)
        "black"
    }
  }

  def direction(x_s: Int, y_s: Int, x_e: Int, y_e: Int, v: Int): Int = {
    if (x_e + v == x_s && y_e - v == y_s)
      0
    else if (x_e - v == x_s && y_e - v == y_s)
      1
    else if (x_e - v == x_s && y_e + v == y_s)
      2
    else
      3
  }

  def moveChecker(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    println(math.abs(y_e - y_s))
    if (canMove(x_s, y_s, x_e, y_e) && math.abs(y_e - y_s) == 1) {
      val value = tab(x_s)(y_s)
      tab(x_e)(y_e) = value
      tab(x_s)(y_s) = 0
    }
  }

  def canMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if ((x_s == x_e || y_s == y_e) ||
        value(x_e, y_e) != 0 || math.abs(y_s - y_e) != math.abs(x_s - x_e))
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
      tab(x_s)(y_s) = 0
      tab((x_s + x_e) / 2)((y_s + y_e) / 2) = 0
    }
  }

  def canStrikeAgain(x_s: Int, y_s: Int): Boolean = {
    val v = value(x_s, y_s)
    if ((value(x_s - 1, y_s + 1) == -v && value(x_s - 2, y_s + 2) == 0) ||
        (value(x_s + 1, y_s + 1) == -v && value(x_s + 2, y_s + 2) == 0) ||
        (value(x_s + 1, y_s - 1) == -v && value(x_s + 2, y_s - 2) == 0) ||
        (value(x_s - 1, y_s - 1) == -v && value(x_s - 2, y_s - 2) == 0))
      true
    else
      false
  }

  def isKing(x: Int, y: Int): Boolean = {
    value(x, y) == 2 || value(x, y) == -2
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
    if (value(cx, cy) == 0) {
      println("Adding 0")
      0
    }
    else if (value(cx, cy) + 1 == -value(x, y) || value(cx, cy) - 1 == -value(x, y)) {
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
      tab(x_s)(y_s) = 0
    }
  }

  def kingStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isKing(x_s, y_s) && canKingStrike(x_s, y_s, x_e, y_e)) {
      for {
        i <- 1 until math.abs(y_e - y_s)
      } yield {
        tab(x_e + i)(y_e + i) = 0
      }
      tab(x_e)(y_e) = value(x_s, y_s)
      tab(x_s)(y_s) = 0
    }
  }

  def move(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isChecker(x_s, y_s))
      if (math.abs(x_s - x_e) == 2) {
        strikeOnce(x_s, y_s, x_e, y_e)
        if (canStrikeAgain(x_e, y_e))
          println("You must strike again, do it!")
      }
      else
        moveChecker(x_s, y_s, x_e, y_e)
    else if (isKing(x_s, y_s)) {
      val strike = canKingMove(x_s, y_s, x_e, y_e)
      println(strike)
      if (!strike) {
        println("King strike!")
        kingStrike(x_s, y_s, x_e, y_e)
      }
      else {
        println("King move!")
        moveKing(x_s, y_s, x_e, y_e)
      }
    }
    else
      println("Error")
  }

  def printing(x: Int, y: Int): Any = {
    if (value(x, y) == 0)
      '-'
    else if (value(x, y) == 1)
      'c'
    else if (value(x, y) == 2)
      'C'
    else if (value(x, y) == -1)
      'b'
    else if (value(x, y) == -2)
      'B'
  }

  def drawBoard(): Unit = {
    for {
      i <- 0 until 8
    } yield {
      for {
        j <- 0 until 8
      } yield {
        print(printing(i, j) + "    ")
        if (j == 7)
          println("\n")
      }
    }
  }

  /*def whereCanStrike(x_s: Int, y_s: Int): Option[List[Int, Int]] = {
    if ( isChecker(x_s, y_s) )
      if ( value(x_s - 1, y_s + 1) == -value(x_s, y_s) && value(x_s - 2, y_s + 2) == 0)

  }*/
}