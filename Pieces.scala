/**
  * Created by cody on 4/12/17.
  */

sealed abstract class Player {
  def op(x: Int): Int
  def opposite: Player
}
case class Black() extends Player {
  def op(x: Int): Int = -x
  def opposite: Player = White()
}
case class White() extends Player {
  def op(x: Int): Int = x
  def opposite: Player = Black()
}

sealed class Loc(var x: Int, var y: Int) {

  override def equals(o: Any): Boolean = {
    o match {
      case that: Loc => {
        if (this.x == that.x && this.y == that.y)
          true
        else
          false
      }
      case _ => false
    }
  }
}

abstract class Piece(p: Player, l: Loc) {
  val funcs: Map[String, State => State]
  val funclist: List[String]
  def getLoc: Loc = this.l
  def getPlayer: Player = this.p
  def getMovLoc(m: String): Loc
}

case class Pawn(p: Player, l: Loc) extends Piece(p,l) {
  val funcs = Map(
    "fwd" -> PartialFunction(fwd),
    "capRight" -> PartialFunction(capRight),
    "capLeft" -> PartialFunction(capLeft)
  )

  val funclist:List[String] = funcs.keys.toList

  def fwd(st: State): State = {
    val pred = PartialFunction(this.l.equals)
    val np = st.pieces.filterNot(pred)
    this.p match {
      case a: Black => this.l.x -= 1
      case a: White => this.l.x += 1
    }
    new State(on_move = this.p.opposite, pieces = np :+ this)
  }

  def capRight(st: State): State = {
    val removeMe = PartialFunction(this.l.equals)
    val removeCap = {
      (mp: Piece) => mp.getPlayer match {
          case a: Black => mp.getLoc.x == (this.l.x - 1) && mp.getLoc.y == (this.l.y + 1)
          case a: White => mp.getLoc.x == (this.l.x + 1) && mp.getLoc.y == (this.l.y + 1)
        }
    }

    val np = st.pieces.filterNot(removeMe).filterNot(removeCap)
    this.p match {
      case a: Black => {
        this.l.x -= 1
        this.l.y += 1
      }
      case a: White => {
        this.l.x += 1
        this.l.y += 1
      }
    }
    new State(on_move = this.p.opposite, pieces = np :+ this)
  }

  def capLeft(st: State): State = {
    val removeMe = PartialFunction(this.l.equals)
    val removeCap = {
      (mp: Piece) => mp.getPlayer match {
        case a: Black => mp.getLoc.x == (this.l.x - 1) && mp.getLoc.y == (this.l.y - 1)
        case a: White => mp.getLoc.x == (this.l.x + 1) && mp.getLoc.y == (this.l.y - 1)
      }
    }
    val np = st.pieces.filterNot(removeMe).filterNot(removeCap)
    this.p match {
      case a: Black => {
        this.l.x -= 1
        this.l.y -= 1
      }
      case a: White => {
        this.l.x += 1
        this.l.y -= 1
      }
    }
    new State(on_move = this.p.opposite, pieces = np :+ this)

  }

  def getMovLoc(m: String): Loc = {
    val newx = this.l.x + this.p.op(1)
    m match {
      case "fwd" => new Loc(x = newx, y = this.l.y)
      case "capRight" => new Loc(x = newx, y = this.l.y + 1)
      case "capLeft" => new Loc(x = newx, y = this.l.y - 1 )
    }
  }
}


