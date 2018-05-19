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

  /*def initTab: Array[Array[Int]] = {
    val t = Array(Array(0, 1, 0, 1, 0, 1, 0, 1),
                  Array(1, 0, 1, 0, 1, 0, 1, 0),
                  Array(0, 1, 0, 1, 0, 1, 0, 1),
                  Array(0, 0, 0, 0, 0, 0, 0, 0),
                  Array(0, 0, 0, 0, 0, 0, 0, 0),
                  Array(-1, 0,-1, 0,-1, 0,-1, 0),
                  Array(0,-1, 0,-1, 0,-1, 0,-1),
                  Array(-1, 0,-1, 0,-1, 0,-1, 0))
     t
  }*/

  def isChecker(x: Int, y: Int): Boolean = {
    if(tab(x)(y) == 1 || tab(x)(y) == -1)
      true
    else
      false
  }

  def move(x_start: Int, y_start: Int, x_end: Int, y_end: Int): Unit = {
    if (isChecker(x_start, y_start)) {
      val value = tab(x_start)(y_start)
      tab(x_end)(y_end) = value
      tab(x_start)(y_start) = 0
    }
  }

  def value(x: Int, y: Int): Int = tab(x)(y)

  def change(x: Int, y: Int, value: Int): Unit = tab(x)(y) = value

  def king(x: Int, y: Int): Unit = {
    if( isChecker(x, y) )
      if( value(x, y) == 1 )
        change(x, y, 2)
      else
        change(x, y, -2)
  }

  def color(x: Int, y: Int): String = {
    if( value(x, y) > 0)
      "white"
    else
      "black"
  }

  /*def canMove(x_s: Int, y_s: Int, x_e: Int, y_e: Int): Boolean = {
    if( value(x_e, y_e) == 1 || value(x_e, y_e) == -1 )
      false
    else if( y_e - 1 == y_s || y_e + 1 == y_s || x_e - 1 == x_s || x_s )
  }*/
}