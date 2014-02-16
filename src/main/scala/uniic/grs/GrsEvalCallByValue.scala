package uniic.grs

import uniic.misc.CompilerException

trait GrsEvalCallByValue extends GrsEval {
  type State = Grs

  def eval(grs0: Grs, debug: Boolean = false)(implicit timeLimit: TimeLimit = NoTimeLimit): Grs = {
    var grs = grs0
    var stepNum = 0
    while (!isValue(grs.rootNode.soleChild)) {
      timeLimit match {
        case MaxSteps(max) if stepNum > max => {
          throw new TimeLimitExceededException
        }
        case NoTimeLimit =>
      }

      if (debug) {
        grs.toGraphviz.writeImageToDebugDir(false)
      }

      grs = stepOne(grs)
    }

    grs
  }

  def isValue(node: GrsNode): Boolean = {
    node.value match {
      case GrsUnit => true
      case _: GrsBool => true
      case _: GrsInt => true
      case _: GrsTuple => node.children.forall(isValue)
      case _: GrsLambda => true
      case _: GrsBuiltinValue => true
      case _: GrsBuiltinFunction => true
      case _ => false
    }
  }

  /** Finds the deepest leftmost redex. */
  def findRedex(n: GrsNode): Option[GrsNode] = {
    if (isValue(n)) {
      None
    } else {
      val reducibleEdge = n.edges.find(e => !e.isLazyEdge && !isValue(e.to))
      Some(reducibleEdge.flatMap(e => findRedex(e.to)).getOrElse(n))
    }
  }

  def stepOne(grs: Grs): Grs = {
    findRedex(grs.rootNode.soleChild) match {
      case Some(redex) => {
        grs.withRedirect(redex, reduce(redex))
      }
      case None => grs
    }
  }

  def reduce(redex: GrsNode): GrsNode = {
    def fail[T](msg: String = null): T = {
      if (msg == null) throw new StuckException("Evaluation got stuck at a " + redex.value + " node")
      else throw new StuckException(msg)
    }

    redex.value match {
      case GrsApply(argCount) => {
        val funNode = redex.children.head
        val argNodes = redex.children.tail
        funNode.value match {
          case fv: GrsBuiltinFunction => fv.impl(argNodes)
          case GrsLambda(_) => {
            funNode.children match {
              case Seq(paramListNode, closureNode, bodyNode) => {
                val bodyGrs = Grs(bodyNode)
                val paramToArg = paramListNode.children.zip(argNodes).toMap

                // We need to ensure the function node is placed back unaltered.
                // If we didn't, a recursive bodyNode could be copied with its
                // ParamList refering to arguments, not variable nodes
                val placeholder = GrsVar("$placeholder")
                val intermediateGrs = bodyGrs.withRedirects(paramToArg + (funNode -> placeholder.toNode))
                val newPlaceholders = intermediateGrs.allNodes.filter(_.value == placeholder)
                val resultGrs = intermediateGrs.withRedirects(newPlaceholders.map(_ -> funNode).toMap)
                resultGrs.rootNode.children.head
              }
            }
          }
          case _ => fail()
        }
      }

      case GrsIfThenElse => {
        redex.children match {
          case Seq(cond, thenBody, elseBody) => {
            cond.value match {
              case GrsBool(true) => thenBody
              case GrsBool(false) => elseBody
              case _ => fail()
            }
          }
        }
      }

      case _: GrsMatch => {
        val headNode = redex.children.head
        val caseNodes = redex.children.tail

        val matchResults = caseNodes.view.map { caseNode =>
          caseNode.value match {
            case GrsCase => {
              caseNode.children match {
                case Seq(patternNode, bodyNode) => {
                  GrsPatternMatcher(patternNode, headNode).map(bodyNode -> _)
                }
              }
            }
            case _ => fail()
          }
        }

        matchResults.flatten.headOption match {
          case Some((body, substs)) => {
            val resultGrs = Grs(body).withRedirects(substs)
            resultGrs.rootNode.children.head
          }
          case None => fail("Pattern match failed")
        }
      }

      case _ => fail()
    }
  }
}
