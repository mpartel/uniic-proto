package uniic.grs

import org.scalatest.FreeSpec
import org.scalatest.matchers.ShouldMatchers
import uniic.types.MissingTypeAnnotation

class GrsScopesTest extends FreeSpec with ShouldMatchers with GrsDSL {
  val MTA = MissingTypeAnnotation

  "declaration sites - lambdas" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'tuple := GrsTuple(2) | ('five, 'lambda),
      'lambda := GrsLambda(Seq(MTA)) | ('x, 'if),
      'if := GrsIfThenElse | ('true, 'x, 'five),
      'true := GrsBool(true),
      'five := GrsInt(5),
      'x := GrsVar("x")
    )

    val x = nodeMap('x).downcast[GrsVar]
    grs.scopes.declarationSiteOf(x) should be (Some(nodeMap('lambda)))
  }

  "declaration sites - pattern cases" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'match := GrsMatch(1) | ('head, 'case),
      'head := GrsTuple(2) | ('five, 'seven),
      'case := GrsCase | ('pattern, 'body),
      'pattern := GrsTuple(2) | ('x, 'y),
      'body := GrsTuple(2) | ('y, 'true),
      'true := GrsBool(true),
      'five := GrsInt(5),
      'seven := GrsInt(7),
      'x := GrsVar("x"),
      'y := GrsVar("y")
    )

    val x = nodeMap('x).downcast[GrsVar]
    val y = nodeMap('y).downcast[GrsVar]
    grs.scopes.declarationSiteOf(x) should be (Some(nodeMap('case)))
    grs.scopes.declarationSiteOf(y) should be (Some(nodeMap('case)))
  }

  "declaration sites - recursive lambdas" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'lambda := GrsLambda(Seq(MTA)) | ('params, 'closure, 'body),
      'params := GrsParamList(1) | ('x),
      'closure := GrsClosure(1) | ('lambda),
      'body := GrsTuple(2) | ('x, 'lambda),
      'x := GrsVar("x")
    )

    val x = nodeMap('x).downcast[GrsVar]
    grs.scopes.declarationSiteOf(x) should be (Some(nodeMap('lambda)))
  }

  "vars in scope - lambdas" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'tuple := GrsTuple(2) | ('outerX, 'lambda),
      'lambda := GrsLambda(Seq(MTA)) | ('innerX, 'if),
      'if := GrsIfThenElse | ('true, 'innerX, 'five),
      'true := GrsBool(true),
      'five := GrsInt(5),
      'innerX := GrsVar("x"),
      'outerX := GrsVar("x")
    )

    val innerX = nodeMap('innerX).downcast[GrsVar]
    val outerX = nodeMap('outerX).downcast[GrsVar]
    grs.scopes.varsInScope(GrsEdge(nodeMap('lambda), 0)) should be (Set(innerX, outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('lambda), 1)) should be (Set(innerX, outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('if), 0)) should be (Set(innerX, outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('if), 1)) should be (Set(innerX, outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('if), 2)) should be (Set(innerX, outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('tuple), 0)) should be (Set(outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('tuple), 1)) should be (Set(outerX))
  }

  "vars in scope - pattern matches" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'match := GrsMatch(1) | ('head, 'case),
      'head := GrsTuple(2) | ('five, 'outerX),
      'case := GrsCase | ('pattern, 'body),
      'pattern := GrsTuple(2) | ('innerX, 'y),
      'body := GrsTuple(2) | ('y, 'outerX),
      'true := GrsBool(true),
      'five := GrsInt(5),
      'seven := GrsInt(7),
      'innerX := GrsVar("x"),
      'outerX := GrsVar("x"),
      'y := GrsVar("y")
    )

    val innerX = nodeMap('innerX).downcast[GrsVar]
    val outerX = nodeMap('outerX).downcast[GrsVar]
    val y = nodeMap('y).downcast[GrsVar]
    grs.scopes.varsInScope(GrsEdge(nodeMap('match), 0)) should be (Set(outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('match), 1)) should be (Set(outerX))
    grs.scopes.varsInScope(GrsEdge(nodeMap('case), 0)) should be (Set(outerX, innerX, y))
    grs.scopes.varsInScope(GrsEdge(nodeMap('case), 1)) should be (Set(outerX, innerX, y))
    grs.scopes.varsInScope(GrsEdge(nodeMap('pattern), 0)) should be (Set(outerX, innerX, y))
    grs.scopes.varsInScope(GrsEdge(nodeMap('pattern), 1)) should be (Set(outerX, innerX, y))
  }

  "all used vars below - simple case" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'tuple := GrsTuple(2) | ('x, 'if),
      'if := GrsIfThenElse | ('true, 'x, 'lambda),
      'lambda := GrsLambda(Seq(MTA)) | ('z, 'y),
      'true := GrsBool(true),
      'x := GrsVar("x"),
      'y := GrsVar("y"),
      'z := GrsVar("z")
    )

    val x = nodeMap('x)
    val y = nodeMap('y)
    val z = nodeMap('z)
    grs.scopes.usedFreeVarsBelow(grs.rootNode) should be (Set(x, y))
    grs.scopes.usedFreeVarsBelow(nodeMap('tuple)) should be (Set(x, y))
    grs.scopes.usedFreeVarsBelow(nodeMap('true)) should be (Set())
    grs.scopes.usedFreeVarsBelow(nodeMap('x)) should be (Set(x))
    grs.scopes.usedFreeVarsBelow(nodeMap('lambda)) should be (Set(y))
    grs.scopes.usedFreeVarsBelow(nodeMap('z)) should be (Set(z))
    grs.scopes.usedFreeVarsBelow(nodeMap('y)) should be (Set(y))
  }

  "all used vars below - spanning a recursive lambda declaration" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'tuple := GrsTuple(2) | ('x, 'lambda),
      'lambda := GrsLambda(Seq(MTA)) | ('y, 'if),
      'if := GrsIfThenElse | ('true, 'y, 'lambda),
      'true := GrsBool(true),
      'x := GrsVar("x"),
      'y := GrsVar("y")
    )

    val x = nodeMap('x)
    val y = nodeMap('y)
    grs.scopes.usedFreeVarsBelow(grs.rootNode) should be (Set(x))
    grs.scopes.usedFreeVarsBelow(nodeMap('tuple)) should be (Set(x))
    grs.scopes.usedFreeVarsBelow(nodeMap('x)) should be (Set(x))
    grs.scopes.usedFreeVarsBelow(nodeMap('lambda)) should be (Set())
    grs.scopes.usedFreeVarsBelow(nodeMap('y)) should be (Set(y))
    grs.scopes.usedFreeVarsBelow(nodeMap('if)) should be (Set(y))
  }

  "all used vars below & vars declared by - spanning a match declaration" in {
    val (grs, nodeMap) = mkGrsAndNodeMap(
      'tuple := GrsTuple(2) | ('y, 'match),
      'match := GrsMatch(1) | ('head, 'case),
      'head := GrsTuple(3) | ('three, 'four, 'five),
      'case := GrsCase | ('pattern, 'body),
      'pattern := GrsTuple(3) | ('x, 'z, 'unused),
      'body := GrsTuple(2) | ('x, 'x),
      'x := GrsVar("x"),
      'y := GrsVar("y"),
      'z := GrsVar("z"),
      'unused := GrsUnused,
      'three := GrsInt(3),
      'four := GrsInt(4),
      'five := GrsInt(5)
    )

    val x = nodeMap('x).downcast[GrsVar]
    val y = nodeMap('y).downcast[GrsVar]
    val z = nodeMap('z).downcast[GrsVar]
    grs.scopes.varsDeclaredBy(grs.rootNode) should be (Set())
    grs.scopes.varsDeclaredBy(nodeMap('case)) should be (Set(x, z))
    grs.scopes.usedFreeVarsBelow(grs.rootNode) should be (Set(y))
    grs.scopes.usedFreeVarsBelow(nodeMap('tuple)) should be (Set(y))
    grs.scopes.usedFreeVarsBelow(nodeMap('match)) should be (Set())
    grs.scopes.usedFreeVarsBelow(nodeMap('head)) should be (Set())
    grs.scopes.usedFreeVarsBelow(nodeMap('case)) should be (Set())
    grs.scopes.usedFreeVarsBelow(nodeMap('body)) should be (Set(x))
  }
}
