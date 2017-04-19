/**
  * Created by cody on 4/12/17.
  */
class State(val on_move: Player, var value: Int = 0, val pieces: List[Piece]){

  override def toString: String = {
    var str = "\nState:\n"
    str += "On Move: " + on_move + "\n"
    str += "Value: " + value + "\n"
    for (p <- pieces){
      str += "Piece: " + p.getPlayer + " " + p.getLoc.x + "," + p.getLoc.y + "\n"
    }

    str
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case s: State => {
        if (this.on_move != s.on_move) {
          //System.err.println("States have different on_move")
          return false
        }
        if (this.value != s.value) {
          //System.err.println("States have different value")
          return false
        }

        if (this.pieces.length != s.pieces.length)
          return false

        for (tp <- this.pieces) {
          val j = s.pieces.find((x: Piece) => x == tp)
          j match {
            case Some(sp) => //System.err.println("matching piece found")
            case _ => return false
          }
        }
        true
      }
      case _ => {
        //System.err.println("Item not a State")
        false
      }
    }
  }
}
