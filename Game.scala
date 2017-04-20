/** Game.scala
  * Cody Shepherd
  */

/** This class represents everything necessary to solve (find the value of)
  * a given initial position.
  * */
class Game(rows: Int = 3, cols: Int = 3, ttenabled: Boolean) {
  var nodecount: Int = 0    //In support of future measurement framework
  var ttcount: Int = 0      //In support of future ttable functionality
  var ttable: Map[String, State] = Map()

  /** This function generates a string representation of the board in the format provided by the tests.
    * */
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

  /** Returns the value of the given state.
    *
    * This is the primary driver function of the game.
    * */
  def solve(s: State): Int = {

    if (ttenabled)
      ttable += makeString(s) -> s

    val result = state_value(s, 0)
    System.err.println("Nodes touched: " + nodecount)
    result
  }

  /** Returns a "palindrome" of a custom string representation of the board, whereby columns are
    * listed from left to right in triplets, with each triplet representing the contents of the column
    * from top to bottom.
    *
    * e.g. the beginning state of a 3x3 board would be "w_bw_bw_b"
    *
    * If black had moved its column-1 piece down, the initial string would be "wb_w_bw_b" and its palindrome
    * would be "w_bw_bwb_"
    * */
  def palindrome(str: String): String = {
    assert(str.length%rows == 0)

    var newstr = ""

    for (i <- List.range(0, str.length, rows)){
      newstr = str.slice(i, i+rows) + newstr
    }
    newstr
  }

  /** Returns a string representation of the board in the format described in the docstring for
    * palindrome().
    * */
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

  /** Returns whether or not a location is within the bounds of the game board.
    * */
  def isInBounds(l:Loc): Boolean = {
    if (l.x < 0 || l.x >= rows)
      false
    else if (l.y < 0 || l.y >= cols)
      false
    else
      true
  }

  /** Returns a list of the locations of all pieces in a given state/board.
    * */
  def getLocs(s: State): List[Loc] = {
    for (p <- s.pieces) yield p.getLoc
  }

  /** Returns whether or not a proposed move from a proposed piece, given
    * a proposed state is legal.
    * */
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

  /** Returns whether the given player is queened (i.e. a pawn is at its opposite row) in
    * the given state.
    * */
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

  /** Returns a (Piece -> List) hash map of all legal moves a list of pieces has in the given state.
    * */
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

  /** Returns the value (-1 or 1; 0 is considered an error) of a given state.
    *
    * Recursive.
    *
    * ii argument is for debugging purposes.
    * */
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
        nodecount += 1

        val new_state = piece.funcs(move)(s)

        /*
        System.err.println("Depth: " + ii + " , " + s.on_move + " moved " + move)
        System.err.println(prettyPrint(new_state))
        scala.io.StdIn.readLine()
        */

        //if(new_state.value == -1)
        if(isQueened(new_state, s.on_move) || new_state.value == -1) {
          new_state.value = -1
          s.value = 1
          return 1
        }

        new_val = -state_value(new_state, ii+1)
        max_val = math.max(max_val, new_val)

        if(max_val == 1){
          return 1
        }
      }
    }

    scala.io.StdIn.readLine()

    s.value = max_val
    max_val
  }

}

