/** Terminal.scala
  * Cody Shepherd
  */

/** This class handles IO for the hexapawn solver. It has no methods, only its
  * constructor loop, which takes in input from StdIn, formats it, greates a
  * Game object, calls its solve() method, and prints the numeric result.
  *
  * This file also happens to hold the runnable program definition for all these
  * files.
  * */
class Terminal {

  var input: List[String] = List()
  System.err.println("Input string")
  for(item <- scala.io.Source.stdin.getLines()) {
    System.err.println(item.toString)
    input = input :+ item.toString
  }

  val p1:String = input.head
  val board:List[String] = input.tail

  val ydim: Int = board.head.length
  val xdim: Int = board.length

  val player:Player = if (p1 == "B") Black() else White()

  var plist: List[Piece] = List[Piece]()

  for((line:String, x:Int) <- board.reverse zip List.range(0, xdim)){
    for((c:Char, y:Int) <- line zip List.range(0, ydim)){
      if (c == 'p')
        plist = plist :+ Pawn(Black(), new Loc(x, y))
      else if (c == 'P')
        plist = plist :+ Pawn(White(), new Loc(x, y))
    }
  }
  val initstate: State = new State(player, 0, plist)

  val g: Game = new Game(xdim, ydim, ttenabled=false)

  print(g.solve(initstate))


}

object Term {
  def main(args: Array[String]): Unit = {
    val t = new Terminal
  }
}
