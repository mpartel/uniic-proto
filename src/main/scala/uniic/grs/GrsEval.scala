package uniic.grs

import scala.annotation.tailrec
import scala.collection.immutable.Stack
import uniic.misc._
import uniic.misc.graphviz.GraphvizGraph
import scala.collection.immutable.LinearSeq

object GrsEval {
  class StuckException(val msg: String, val state: State) extends CompilerException(msg)
  class TimeLimitExceededException extends CompilerException("Maximum evaluation steps exceeded")

  sealed trait TimeLimit
  case object NoTimeLimit extends TimeLimit
  case class MaxSteps(steps: Int) extends TimeLimit

  def eval(grs: Grs, debug: Boolean = false)(implicit timeLimit: TimeLimit = NoTimeLimit): Grs = {
    var state = State(grs, List(StackElement(LinearSeq.empty, 0)), true, 1)
    while (!state.isFinished) {
      timeLimit match {
        case MaxSteps(max) if state.stepNum > max => {
          throw new TimeLimitExceededException
        }
        case NoTimeLimit =>
      }

      if (debug) {
        state.toGraphviz.writeImageToDebugDir()
      }

      state = stepOne(state)
    }

    state.grs
  }

  case class StackElement(path: LinearSeq[Int], nextChildIndex: Int) {
    def currentNode(grs: Grs): GrsNode = {
      path.foldLeft(grs.rootNode: GrsNode)(_.children(_))
    }

    def next = StackElement(path :+ nextChildIndex, 0)
  }

  case class State(grs: Grs, stack: List[StackElement], isDescending: Boolean, stepNum: Int) {
    def isFinished = stack.isEmpty

    def currentNode = stack.head.currentNode(grs)
    def nextChildIndex = stack.head.nextChildIndex

    def advanceIndex = setIndex(nextChildIndex + 1)
    def resetIndex = setIndex(0)
    private def setIndex(i: Int) = {
      this.copy(stack = stack.head.copy(nextChildIndex = i) :: stack.tail)
    }

    def descendToNextChild = this.copy(stack = stack.head.next :: stack)
    def pop = this.copy(stack = stack.tail)

    def markDescending = this.copy(isDescending = true)
    def markAscending = this.copy(isDescending = false)

    def replaceGrs(newGrs: Grs) = this.copy(grs = newGrs)

    def countStep = this.copy(stepNum = stepNum + 1)

    def stackContains(node: GrsNode) = stack.exists(_.currentNode(grs) == node)

    def toGraphvizNode = {
      val title = if (isDescending) "Stack (descending)" else "Stack"
      val header = s"$title - step $stepNum"
      val stackEntries = stack.map(e => e.currentNode(grs).toString + "  " + e.nextChildIndex)
      val label = stackEntries.mkString(header + ":\\l", "\\l", "\\l")
      val node = new GraphvizGraph.Node(label)
      node.attributes("shape") = "rectangle"
      node
    }

    def toGraphviz = {
      val paddedStepNum = stepNum.toString.reverse.padTo(3, '0').reverse
      val gg = grs.toGraphviz.addNode(this.toGraphvizNode).setName("GrsEval_" + paddedStepNum)

      if (!stack.isEmpty) {
        gg.findNode(currentNode).foreach { _.attributes("color") = "red" }
        gg.findEdge(GrsEdge(currentNode, nextChildIndex)).foreach { e =>
          if (isDescending) {
            e.attributes("color") = "green"
          } else {
            e.attributes("dir") = "back"
            e.attributes("color") = "blue"
          }
        }
      }

      gg
    }
  }

  def stepOne(state: State): State = {
    if (state.isFinished) {
      return state
    }

    val currentNode = state.currentNode
    val nextChildIndex = state.nextChildIndex

    def unvisitedChildren = nextChildIndex < currentNode.value.arity

    val newState = {
      if (state.isDescending) {
        def alreadyVisited = state.stackContains(currentNode.children(nextChildIndex))
        if (unvisitedChildren && !alreadyVisited) {
          if (currentNode.value.isLazyEdge(nextChildIndex)) {
            state.advanceIndex
          } else {
            state.descendToNextChild
          }
        } else {
          reduceOrUnwind(state)
        }
      } else {
        if (unvisitedChildren) {
          state.advanceIndex.markDescending
        } else {
          reduceOrUnwind(state)
        }
      }
    }

    newState.countStep
  }

  private def reduceOrUnwind(state: State): State = {
    getReductionIfReducable(state) match {
      case Some(resultNode) => {
        val newGrs = state.grs.withRedirect(state.currentNode, resultNode)
        state.replaceGrs(newGrs).resetIndex.markDescending
      }
      case None => {
        state.pop.markAscending
      }
    }
  }

  private def getReductionIfReducable(state: State): Option[GrsNode] = {
    val currentNode = state.currentNode

    def fail[T](msg: String = null): T = {
      if (msg == null) throw new StuckException("Evaluation got stuck", state)
      else throw new StuckException(msg, state)
    }

    currentNode.value match {
      case GrsApply(argCount) => {
        val funNode = currentNode.children.head
        val argNodes = currentNode.children.tail
        funNode.value match {
          case fv: GrsBuiltinFunction => {
            Some(fv.impl(argNodes))
          }
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
                Some(resultGrs.rootNode.children.head)
              }
            }
          }
          case _ => fail()
        }
      }

      case GrsIfThenElse => {
        currentNode.children match {
          case Seq(cond, thenBody, elseBody) => {
            cond.value match {
              case GrsBool(true) => Some(thenBody)
              case GrsBool(false) => Some(elseBody)
              case _ => fail()
            }
          }
        }
      }

      case _: GrsMatch => {
        val headNode = currentNode.children.head
        val caseNodes = currentNode.children.tail

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
            Some(resultGrs.rootNode.children.head)
          }
          case None => fail("Pattern match failed")
        }
      }

      case _ => None
    }
  }
}
