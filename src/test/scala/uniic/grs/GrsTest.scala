package uniic.grs

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.test._

class GrsTest extends FreeSpec with ShouldMatchers with GrsTestUtils {
  "equivalence" in {
    val g1 = {
      val a = GrsInt(1).toNode
      lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a, b))
      Grs(GrsRoot.toNode(Seq(b)))
    }
    val g2 = {
      val a = GrsInt(1).toNode
      val b = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a, a))
      Grs(GrsRoot.toNode(Seq(b)))
    }
    val g3 = {
      val a = GrsInt(1).toNode
      lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a, b))
      Grs(GrsRoot.toNode(Seq(b)))
    }
    val g4 = {
      val a = GrsInt(1000).toNode
      lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a, b))
      Grs(GrsRoot.toNode(Seq(b)))
    }

    g1 should beEquivalentTo (g1)
    g2 should beEquivalentTo (g2)
    g3 should beEquivalentTo (g3)
    g4 should beEquivalentTo (g4)

    g1 should not (beEquivalentTo (g2))
    g1 should beEquivalentTo (g3)
    g1 should not (beEquivalentTo (g4))
  }

  "redirecting" - {
    "at a self-referential node" in {
      val out = {
        val a = GrsInt(1).toNode
        lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a, b))
        val c = GrsInt(2).toNode
        val in = Grs(GrsRoot.toNode(Seq(b)))
        in.withRedirect(a, b)
      }

      val expected = {
        lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(b, b))
        Grs(GrsRoot.toNode(Seq(b)))
      }

      out should beEquivalentTo (expected)
    }

    "at a cycle" in {
      val out = {
        val a1 = GrsInt(1).toNode
        val a2 = GrsInt(2).toNode
        val a3 = GrsInt(3).toNode
        lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a1, c))
        lazy val c: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a2, d))
        lazy val d: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a3, b))
        val in = Grs(GrsRoot.toNode(Seq(b)))
        in.withRedirect(c, d)
      }

      val (expected, notExpected) = {
        val a1 = GrsInt(1).toNode
        val a2 = GrsInt(2).toNode
        val a3 = GrsInt(3).toNode
        lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a1, d))
        lazy val d: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a3, b))
        val good = Grs(GrsRoot.toNode(Seq(b)))
        val bad = Grs(GrsRoot.toNode(Seq(d)))
        (good, bad)
      }

      out should beEquivalentTo (expected)
      out should not (beEquivalentTo (notExpected))
    }

    "at a diamond shape" in {
      val out = {
        val a1 = GrsInt(1).toNode
        val a2 = GrsInt(2).toNode
        val a3 = GrsInt(3).toNode
        lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(c, d))
        lazy val c: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a1, e))
        lazy val d: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(e, a1))
        lazy val e: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a2, a3))
        lazy val e2: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a3, a2))
        val in = Grs(GrsRoot.toNode(Seq(b)))
        in.withRedirect(e, e2)
      }

      val expected = {
        val a1 = GrsInt(1).toNode
        val a2 = GrsInt(2).toNode
        val a3 = GrsInt(3).toNode
        lazy val b: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(c, d))
        lazy val c: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a1, e2))
        lazy val d: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(e2, a1))
        lazy val e2: GrsNode = GrsTuple(2).toNodeWithLazyChildren(() => Seq(a3, a2))
        Grs(GrsRoot.toNode(Seq(b)))
      }

      out should beEquivalentTo (expected)
    }
  }
}
