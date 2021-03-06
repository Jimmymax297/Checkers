/**
  * Class made to contain lists of movements.
  * @param l - list of tuples
  */
class Movement(l: List[(Int, Int, Int, Int)]) {
  val move: List[(Int, Int, Int, Int)] = l

  /**
    * Function to print movement paths
    */
  def print():Unit = {
    val l = move.length
    for{
      i <- 0 until l
    } yield {
      val fromX = move(i)._1
      val fromY = move(i)._2
      val toX = move(i)._3
      val toY = move(i)._4
      println(" ( " + fromX + " , " + fromY + " ) " + " -> " + " ( " + toX + " , " + toY + " ) ")
    }
  }
}
