import scala.runtime.Nothing$

/**
  * Created by cody on 4/12/17.
  */
class Game(rows: Int = 3, cols: Int = 3, ttenabled: Boolean) {
  val state_invariant = new State(on_move = White(), pieces =
      (for (c <- List.range(0,cols)) yield Pawn(p = White(), l = new Loc(0,c))) :::
      (for (c <- List.range(0,cols)) yield Pawn(p = Black(), l = new Loc(rows-1, c))))
  var nodecount: Int = 0
  var ttcount: Int = 0
  var ttable: Map[String, State] = Map{
    "w_bw_bw_b" -> new State(on_move = White(), pieces =
      (for (c <- List.range(0,cols)) yield Pawn(p = White(), l = new Loc(0,c))) :::
      (for (c <- List.range(0,cols)) yield Pawn(p = Black(), l = new Loc(rows-1, c))))
  }

  def prettyPrint(s: State): Boolean = {
    System.err.println("PrettyPrint's State: " + s)
    var str = ""
    var tempstr = ""
    for (r <- List.range(0, rows)){
      for (c <- List.range(0, cols)) {
        val pc = s.pieces.find((x:Piece) => if (x.getLoc.x == r && x.getLoc.y == c) true else false)
        pc match {
          case Some(a) => if (a.getPlayer == White()) tempstr += "[w]" else tempstr += "[b]"
          case _ => tempstr += "[ ]"
        }
      }
      str = tempstr + '\n' + str
      tempstr = ""
    }

    System.err.println(str)
    true
  }

  def solve(): Int = {
    val initState = ttable.get("w_bw_bw_b")
    initState match {
      case Some(a) => state_value(a, 0)
      case _ => 0
    }
  }

  def palindrome(str: String): String = {
    assert(str.length%rows == 0)

    var newstr = ""

    for (i <- List.range(0, str.length, rows)){
      newstr = str.slice(i, i+rows) + newstr
    }
    newstr
  }

  def makeString(s: State): String = {
    var str = ""
    for (c <- List.range(0, cols)){
      for (r <- List.range(0, rows)) {
        val pc = s.pieces.find((x:Piece) => if (x.getLoc.x == r && x.getLoc.y == c) true else false)
        pc match {
          case Some(a) => if (a.getPlayer == White()) str += "w" else str += "b"
          case _ => str += "_"
        }
      }
    }
    str
  }

  def isInBounds(l:Loc): Boolean = {
    if (l.x < 0 || l.x >= rows)
      false
    else if (l.y < 0 || l.y >= cols)
      false
    else
      true
  }

  def getLocs(s: State): List[Loc] = {
    for (p <- s.pieces) yield p.getLoc
  }

  def checkMove(f: String, p: Piece, s: State): Boolean = {
    assert(p.funcs.contains(f))

    val newLoc = p.getMovLoc(f)
    //System.err.println("New loc calculated by getMovLoc: " + newLoc.x + "," + newLoc.y)
    if (!isInBounds(newLoc))
      return false

    val ptrn = "^cap.*".r
    val atLoc = s.pieces.find((a: Piece) => a.getLoc == newLoc)
    //System.err.println("Pieces found at " + newLoc.x + "," + newLoc.y + ": " + atLoc)

    f match {
      case ptrn() => {
        atLoc match {
          case Some(b) => {
            //System.err.println("At attempted location: " + b.getPlayer)
            if (b.getPlayer != p.getPlayer)
              return true
            false
          }
          case _ => false
        }
      }
      case "fwd" => {
        atLoc match {
          case Some(b) => false
          case _ => true
        }
      }
        //Add more cases here for different pieces
    }

  }

  def isQueened(s: State): Boolean = {
    val p = s.on_move
    p match {
      case a: Black => {
        val xs = for (item <- s.pieces.filter((pc: Piece) => pc.getPlayer == a)) yield item.getLoc.x
        if (xs.contains(0))
          true
        else
          false
      }
      case a: White => {
        val xs = for (item <- s.pieces.filter((pc: Piece) => pc.getPlayer == a)) yield item.getLoc.x
        if (xs.contains(rows-1))
          true
        else
          false
      }
    }
  }

  def state_value(s: State, ii: Int): Int = {

    System.err.println("\n======================================\n")
    System.err.println("Layer " + ii)
    System.err.println("State: " + s)
    System.err.println("On move: " + s.on_move)

    if (isQueened(s)) {
      if (ii==0) assert(s == state_invariant)
      //s.value = 1
      return 1
    }
    if (ii==0) assert(s == state_invariant)

    val p = s.on_move
    var retVal = -1
    if (ii==0) assert(s == state_invariant)

    //Memoization, ttable checking
    val key = makeString(s)
    if (ii==0) assert(s == state_invariant)
    var foundState = ttable.get(key)
    foundState match {
      case Some(st) => {
        if (st.value != 0) {
          System.err.println("==== Found a state in ttable; returning ====")
          return st.value
        }
      }
      case _ =>
    }

    val pkey = palindrome(key)
    foundState = ttable.get(pkey)
    foundState match {
      case Some(st) => {
        if (st.value != 0) {
          System.err.println("==== Found a palindrome state in ttable; returning ====")
          return st.value
        }
      }
      case _ =>
    }

    val onMovePieces = s.pieces.filter((a:Piece) => a.getPlayer == p)
    if (ii==0) assert(s == state_invariant)
    for (piece <- onMovePieces){
      if (ii==0) assert(s == state_invariant)

      prettyPrint(s)
      System.err.println("^Layer " + ii)
      System.err.println("On move: " + s.on_move)
      //System.err.println(s)
      System.err.println("Pieces: " + onMovePieces )
      System.err.println("At piece: " + piece)
      System.err.println("Piece Details: " + piece.getPlayer + piece.getLoc.x + ',' + piece.getLoc.y)
      scala.io.StdIn.readLine()

      for (move <- piece.funclist){
        val checked = checkMove(move, piece, s)
        if (ii==0) assert(s == state_invariant)

        System.err.println("\nMove Chosen: " + move)
        System.err.println("Is it legal? " + checked)

        if (checked) {
          val passDown = piece.funcs(move)(s)
          if (ii==0) assert(s==state_invariant)

          val ans = state_value(passDown, ii+1)
          if (ii==0) assert(s == state_invariant)

          retVal = math.max(retVal, -ans)

          //win-prune
          //if (retVal == 1)
          //  return retVal

          System.err.println("Returned from recursive layer " + (ii+1))
          System.err.println("result: " + retVal)
          if (ii==0) assert(s == state_invariant)
        }
      }
    }
    //s.value = retVal
    val ns = new State(on_move = s.on_move, value = retVal, pieces = for (p <- s.pieces) yield p)
    ttable += makeString(ns) -> ns
    if (ii==0) assert(s == state_invariant)
    retVal
  }

}

object Program {
  def main(args: Array[String]): Unit = {
    val g = new Game(ttenabled = true)
    print (g.solve())

    /*
    val p = Pawn(p=White(), l=new Loc(0,0))
    println("len p.funclist: " + p.funclist.length)
    println(p.funclist)
    */

    /*
    val ptt = "^cap.*".r
    val mystr = "capLeft"
    mystr match{
      case ptt() => print("its matching")
      case _ => print ("Its not matching")
    }
    */
  }
}

