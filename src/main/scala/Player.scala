class Player(b: Board, c: String) {
  val color: String = c
  def makeMove(): Unit = {
    val in1 = scala.io.StdIn.readInt()
    val in2 = scala.io.StdIn.readInt()
    val in3 = scala.io.StdIn.readInt()
    val in4 = scala.io.StdIn.readInt()
    if (!checkInput(in1) || !checkInput(in2) || !checkInput(in3) || !checkInput(in4))
      println("Wrong movement, do it again")
    b.move(in1, in2, in3, in4)
  }

  def checkInput(i: Int): Boolean = i >= 0 && i <= 7
}
