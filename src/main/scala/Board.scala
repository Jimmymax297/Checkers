class Board {
  var tab:Array[Array[Int]] = {
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

  def color(x: Int, y: Int): String = {
    if (value(x, y) > 0)
      "white"
    else
      "black"
  }

  def direction(x_s: Int, y_s: Int, x_e: Int, y_e: Int, v: Int): AnyVal = {
    if (x_e + v == x_s && y_e - v == y_s)
      0
    else if (x_e - v == x_s && y_e - v == y_s)
      1
    else if (x_e - v == x_s && y_e + v == y_s)
      2
    else if (x_e + v == x_s && y_e + v == y_s)
      3
  }

  def move(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Unit = {
    if (isChecker(x_s, y_s) && canMove(x_s, y_s, x_e, y_e)) {
      val value = tab(x_s)(y_s)
      tab(x_e)(y_e) = value
      tab(x_s)(y_s) = 0
    }
  }

  def canMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if ((x_s == x_e || y_s == y_e) || (value(x_e, y_e) == 1 || value(x_e, y_e) == -1))
      false
    else
      true
  }

  def canStrike(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if (isChecker(x_s, y_s) && canMove(x_s, y_s, x_e, y_s) && value((x_s + x_e) / 2, (y_s + y_e) / 2) == -value(x_s, y_s))
      true
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

  /*def canStrikeAgain(x_s: Int, y_s: Int): Boolean = {
    if ()
  }*/

  /*def whereCanStrike(x_s: Int, y_s: Int): Option[List[Int, Int]] = {
    if ( isChecker(x_s, y_s) )
      if ( value(x_s - 1, y_s + 1) == -value(x_s, y_s) && value(x_s - 2, y_s + 2) == 0)

  }*/
}