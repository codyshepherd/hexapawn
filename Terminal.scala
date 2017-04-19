/**
  * Created by cody on 4/19/17.
  */
class Terminal {

  for(item <- scala.io.Source.stdin.getLines())
    System.err.println(item.toString)


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
