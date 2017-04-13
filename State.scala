/**
  * Created by cody on 4/12/17.
  */
class State(val on_move: Player, val value: Int = 0, val pieces: List[Piece]){

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
          System.out.println("States have different on_move")
          return false
        }
        if (this.value != s.value) {
          System.out.println("States have different value")
          return false
        }
        for ((tp, sp) <- this.pieces zip s.pieces){
          if (tp.getPlayer != sp.getPlayer) {
            System.out.println("Pieces.player different somewhere in State lists")
            return false
          }
          else if (tp.getLoc != sp.getLoc) {
            System.out.println("Pieces.loc different somewhere in State lists")
            System.out.println(tp.getLoc.x + "," + tp.getLoc.y + " vs " + sp.getLoc.x + "," + sp.getLoc.y)
            return false
          }
        }
        true
      }
      case _ => {
        System.out.println("Item not a State")
        false
      }
    }
  }
}
