/**
  * Created by cody on 4/19/17.
  */
class Test {
  val l = List(
    Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
    Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2))
  )
  val s = new State(Black(), 0, l)

  def testBlack(): Unit ={
    val b = Black()
    assert(b.opposite == White())
    assert(b.op(1) == -1)
    assert(b.op(-1) == 1)
  }

  def testWhite(): Unit ={
    val w = White()
    assert(w.opposite == Black())
    assert(w.op(1) == 1)
    assert(w.op(-1) == -1)
  }

  def testLoc(): Unit ={
    val l = new Loc(1,2)
    assert(l == new Loc(1,2))
    assert(l != new Loc(1,1))
    assert(l != (1,2))
    assert(l.x == 1)
    assert(l.y == 2)
    assert(l.x != 2)
    assert(l.y != 1)
  }

  def testState(): Unit ={
    val la = List(
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2))
    )

    val lb = List(
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2))
    )

    val lc = List(
      Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)), Pawn(Black(), new Loc(2,0)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2))
    )

    val ld = List(
      Pawn(Black(), new Loc(1,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2))
    )

    val sa = new State(White(), 0, la)
    val sap = new State(Black(), 0, la)
    val sb = new State(White(), 0, lb)
    val sc = new State(White(), 0, lc)
    val sd = new State(White(), 0, ld)

    assert(sa == sa)
    assert(sa == sb)
    assert(sa != sap)

    assert(sa == sc)

    assert(sa != sd)
  }

  def testPawnBlack(): Unit = {
    val l = List(
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2))
    )
    val al = List(
      Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(1,0))
    )
    val bl = List(
      Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(1,1))

    )
    val cl = List(
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)),
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(1,1))

    )
    val s = new State(Black(), 0, l)
    val invar = new State(Black(), 0, l)
    val as = new State(White(), 0, al)
    val bs = new State(White(), 0, bl)
    val cs = new State(White(), 0, cl)

    val p = l.head
    val np = l.find((p:Pawn) => p.getLoc.x == 2 && p.getLoc.y == 2)

    assert(p.p == Black())
    assert(p.getPlayer == Black())

    assert(p.l == new Loc(2,0))
    assert(p.getLoc == new Loc(2,0))

    val ns = p.funcs("fwd")(s)

    assert(ns != s)
    assert(ns == as)
    assert(ns != invar)
    assert(s == invar)

    val ms = p.funcs("capRight")(s)

    assert(ms != s)
    assert(ms == bs)
    assert(ms != invar)
    assert(s == invar)

    //val ls = np.funcs("capLeft")(s)
    np match {
      case Some(a) => {
        val ls = a.funcs("capLeft")(s)
        assert (ls != s)
        assert (ls == cs)
        assert (ls != invar)
        assert (s == invar)
      }
      case _ => assert(false)
    }
  }

  def testPawnWhite(): Unit ={
    val l = List(
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2))
    )
    val al = List(
      Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(1,0))
    )
    val bl = List(
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)),
      Pawn(White(), new Loc(2,2))
    )
    val cl = List(
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(2,0))
    )
    val s = new State(White(), 0, l)
    val invar = new State(White(), 0, l)
    val as = new State(Black(), 0, al)
    val bs = new State(Black(), 0, bl)
    val cs = new State(Black(), 0, cl)

    val p = l.head
    val np = l.find((p:Pawn) => p.getLoc.x == 1 && p.getLoc.y == 1)

    assert(p.p == White())
    assert(p.getPlayer == White())

    assert(p.l == new Loc(0,0))
    assert(p.getLoc == new Loc(0,0))

    val ns = p.funcs("fwd")(s)

    assert(ns != s)
    assert(ns == as)
    assert(ns != invar)
    assert(s == invar)

    np match {
      case Some(a) => {
        val ms = a.funcs("capRight")(s)

        assert(ms != s)
        assert(ms == bs)
        assert(ms != invar)
        assert(s == invar)

        val ls = a.funcs("capLeft")(s)
        assert (ls != s)
        assert (ls == cs)
        assert (ls != invar)
        assert (s == invar)
      }
      case _ => assert(false)
    }

  }

  def testGame(): Unit = {
    val g = new Game(ttenabled=true)
    val l = List(
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2))
    )
    val al = List(
      Pawn(White(), new Loc(1,1)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(1,0))
    )
    val bl = List(
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,0)), Pawn(Black(), new Loc(2,1)),
      Pawn(White(), new Loc(2,2))
    )
    val cl = List(
      Pawn(White(), new Loc(0,0)), Pawn(White(), new Loc(0,2)),
      Pawn(Black(), new Loc(2,1)), Pawn(Black(), new Loc(2,2)),
      Pawn(White(), new Loc(2,0))
    )
    val s = new State(White(), 0, l)
    val invar = new State(White(), 0, l)
    val as = new State(Black(), 0, al)
    val bs = new State(Black(), 0, bl)
    val cs = new State(White(), 0, cl)

    def testPrettyPrint(): Unit ={
      assert(g.prettyPrint(s) == "W\nppp\n.P.\nP.P\n")
      assert(g.prettyPrint(as) == "B\nppp\nPP.\n..P\n")
      assert(g.prettyPrint(bs) == "B\nppP\n...\nP.P\n")
      assert(g.prettyPrint(cs) == "W\nPpp\n...\nP.P\n")
    }

    def testMakeString(): Unit = {
      assert(g.makeString(s) == "w_b_wbw_b")
      assert(g.makeString(bs) == "w_b__bw_w")
    }

    def testPalindrome(): Unit ={
      assert(g.makeString(bs) == g.palindrome(g.palindrome(g.makeString(bs))))
      assert(g.palindrome(g.makeString(bs)) == "w_w__bw_b")
    }

    def testIsInBounds(): Unit ={
      assert(!g.isInBounds(new Loc(3,0)))
      assert(!g.isInBounds(new Loc(0,-1)))
      assert(g.isInBounds(new Loc(2,2)))
      assert(g.isInBounds(new Loc(0,0)))
    }

    def testGetLocs(): Unit ={
      val locs: List[Loc] = List(
        new Loc(0,0), new Loc(1,1), new Loc(0,2),new Loc(2,0), new Loc(2,1), new Loc(2,2)
      )

      val alocs: List[Loc] = List(new Loc(1,1), new Loc(0,2), new Loc(2,0), new Loc(2,1), new Loc(2,2), new Loc(1,0))

      assert(g.getLocs(s) == locs)
      assert(g.getLocs(as) == alocs)
    }

    def testCheckMove(): Unit ={
      assert(!g.checkMove("capRight", l.head, s))
      assert(!g.checkMove("capLeft", l.head, s))
      assert(g.checkMove("fwd", l.head, s))

      assert(g.checkMove("capRight", l(1), s))
      assert(g.checkMove("capLeft", l(1), s))
      assert(!g.checkMove("fwd", l(1), s))

      assert(!g.checkMove("capRight", bl(2), bs))
      assert(!g.checkMove("capLeft", bl(2), bs))
      assert(g.checkMove("fwd", bl(2), bs))
    }

    def testIsQueened(): Unit ={
      assert(!g.isQueened(s, White()))
      assert(!g.isQueened(bs, Black()))
      assert(g.isQueened(cs, White()))
    }

    testPrettyPrint()
    testMakeString()
    testPalindrome()
    testIsInBounds()
    testGetLocs()
    testCheckMove()
    testIsQueened()
  }

  def testAll(): Boolean = {
    testBlack()
    testWhite()
    testLoc()
    testState()
    testPawnWhite()
    testPawnBlack()
    testGame()
    true
  }
}

object Testing {
  def main(args: Array[String]): Unit = {
    val t = new Test()
    print(t.testAll())
  }
}
