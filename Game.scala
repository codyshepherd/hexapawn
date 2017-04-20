/**
  * Created by cody on 4/12/17.
  */
class Game(rows: Int = 3, cols: Int = 3, ttenabled: Boolean) {
  var nodecount: Int = 0
  var ttcount: Int = 0
  var ttable: Map[String, State] = Map{
    "w_bw_bw_b" -> new State(on_move = White(), pieces =
      (for (c <- List.range(0,cols)) yield Pawn(p = White(), l = new Loc(0,c))) :::
      (for (c <- List.range(0,cols)) yield Pawn(p = Black(), l = new Loc(rows-1, c))))
  }

  def prettyPrint(s: State): String = {
    var str = ""

    var tempstr = ""
    for (r <- List.range(0, rows)){
      for (c <- List.range(0, cols)) {
        val pc = s.pieces.find((x:Piece) => if (x.getLoc.x == r && x.getLoc.y == c) true else false)
        pc match {
          case Some(a) => if (a.getPlayer == White()) tempstr += "P" else tempstr += "p"
          case _ => tempstr += "."
        }
      }
      str = tempstr + '\n' + str
      tempstr = ""
    }

    if(s.on_move == White())
      str = "W\n" + str
    else
      str = "B\n" + str

    str
  }

  def solve(s: State): Int = {

    if (ttenabled)
      ttable += makeString(s) -> s
    state_value(s, 0)

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

  def isQueened(s: State, p: Player): Boolean = {
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

  def getLegalMoves(pieces:List[Piece], s:State): Map[Piece, List[String]] = {
    var m: Map[Piece, List[String]] = Map[Piece, List[String]]()
    var yn = false
    for(p <- pieces) {
      for (funcstring <- p.funclist) {
        yn = checkMove(funcstring, p, s)
        if (yn)
          m.get(p) match {
            case Some(xs:List[String]) => m = m.updated(p, xs :+ funcstring)
            case None => m = m.updated(p, List(funcstring))
          }
      }
    }
    m
  }

  def state_value(s: State, ii: Int): Int = {
    //if(ii == 2)
    //  return s.value

    val on_move_pieces  :List[Piece]               = s.pieces.filter((p:Piece) => p.getPlayer == s.on_move)
    val legal_moves     :Map[Piece, List[String]]  = getLegalMoves(on_move_pieces, s)

    var max_val = -1
    var new_val = -99

    if(legal_moves.isEmpty) {
      s.value = -1
      return -1
    }

    for(piece: Piece <- legal_moves.keys) {
      for (move: String <- legal_moves(piece)){
        val new_state = piece.funcs(move)(s)

        /*
        System.err.println("Depth: " + ii + " , " + s.on_move + " moved " + move)
        System.err.println(prettyPrint(new_state))
        scala.io.StdIn.readLine()
        */

        //if(new_state.value == -1)
        if(isQueened(new_state, s.on_move)) {
          new_state.value = -1
          new_val = 1
        }
        else
          new_val = -state_value(new_state, ii+1)
        max_val = math.max(max_val, new_val)
      }
    }

    scala.io.StdIn.readLine()

    s.value = max_val
    max_val
  }

}

