/**
  * Created by cody on 4/19/17.
  */
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

    /*
    p1 match {
    case "B" => Black()
    case _ => White()
  }
  */

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

  //System.err.println("Init state:")
  //System.err.println(g.prettyPrint(initstate))

  print(g.solve(initstate))


  /*
  val onmove: String = scala.io.StdIn.readLine()

  if(onmove.length > 1)
    System.exit(-1)

  var board: String = ""
  var countB: Int = 0
  var countW: Int = 0

  var line: String = scala.io.StdIn.readLine()
  val len: Int = line.length
  countB += line.count((p:Char) => p == 'p')
  countW += line.count((p:Char) => p == 'P')

  while(countB < len && countW < len) {
    board += line
    line = scala.io.StdIn.readLine()
  }

  val lines: List[String] = board.split("\n").toList
  System.err.println(onmove)
  for(line <- lines)
    System.err.println(line)
   */

}

object Term {
  def main(args: Array[String]): Unit = {
    val t = new Terminal
  }
}
