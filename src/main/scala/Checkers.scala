class Checkers(WPlayer: Player ,Bplayer: Player){
  val WhitePlayer :Player = WPlayer
  val BlackPlayer :Player = Bplayer
  def start:Unit = {}
  def printTest = {
    WhitePlayer.printTest()
    BlackPlayer.printTest()
  }
}
