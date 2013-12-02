package uniic.flow

import uniic.misc._

private[flow] class FlowGraphParsers extends scala.util.parsing.combinator.Parsers {
  // We'd like to say Elem = FlowStmt but Scala's parser combinators fail to check
  // Reader.atEnd before calling Reader.first. Essentially this means that Elem
  // must be able to represent an EOF. We'll use None for EOF.
  type Elem = Option[FlowStmt]

  def flowGraph: Parser[FlowGraph] = {
    rep1(blockWithLabel) ^^ { blocks =>
      val firstLabel = blocks.head._1
      new FlowGraph(blocks, firstLabel)
    }
  }

  def blockWithLabel: Parser[(FlowLabel, FlowBlock)] = (
    label ~ blockAfterLabel ^^ { case l ~ b => (l, b) }
  )

  def blockAfterLabel: Parser[FlowBlock] = (
    rep(phi) ~ rep(basic) ~ jump ^^
      { case ps ~ bs ~ j => FlowBlock(ps, bs, j) }
  )

  def label: Parser[FlowLabel] = (
    acceptMatch("label", { case Some(lbl: FlowLabel) => lbl })
  )
  def phi: Parser[FlowPhiStmt] = (
    acceptMatch("phi statement", { case Some(phi: FlowPhiStmt) => phi })
  )
  def basic: Parser[FlowBasicStmt] = (
    acceptMatch("basic statement", { case Some(basic: FlowBasicStmt) => basic })
  )
  def jump: Parser[FlowJumpStmt] = (
    acceptMatch("jump statement", { case Some(jump: FlowJumpStmt) => jump })
  )
}

private[flow] object FlowGraphParsers {
  private val parsers = new FlowGraphParsers

  def parseGraph(input: Seq[FlowStmt]): FlowGraph = {
    parse(input, parsers.flowGraph)
  }

  def parseBlockWithLabel(input: Seq[FlowStmt]): (FlowLabel, FlowBlock) = {
    parse(input, parsers.blockWithLabel)
  }

  def parseBlockAfterLabel(input: Seq[FlowStmt]): FlowBlock = {
    parse(input, parsers.blockAfterLabel)
  }

  private def parse[A](input: Seq[FlowStmt], parser: parsers.Parser[A]): A = {
    import scala.collection.immutable.PagedSeq
    import scala.util.parsing.input.PagedSeqReader
    import uniic.parsers.SeqReader


    val inputReader = SeqReader(input.map(Some(_)).toList, None)
    parsers.phrase(parser)(inputReader) match {
      case parsers.Success(result, _) => result
      case parsers.NoSuccess(msg, next) => {
        throw new CompilerException(
          "Failed to compile flow statements into basic blocks: " + msg +
          " at " + Inflections.ordinal(next.pos.line) + " statement"
        )
      }
    }
  }
}
