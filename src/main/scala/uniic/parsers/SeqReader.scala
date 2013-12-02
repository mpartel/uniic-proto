package uniic.parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.collection.LinearSeq

/** A parsing input that simply reads a LinearSeq of any type.
  *
  * Because Scala's parser combinators don't always check atEnd before calling first,
  * there must be a special EOF value. */
class SeqReader[+T](val seq: LinearSeq[T], val pos: SeqReader.SeqPosition[T], val eof: T) extends Reader[T] {
  def atEnd = seq.isEmpty
  def first = if (seq.isEmpty) eof else seq.head
  def rest = new SeqReader(seq.tail, new SeqReader.SeqPosition(pos.line + 1, seq.tail.headOption), eof)
}

object SeqReader {
  def apply[T](seq: LinearSeq[T], eof: T) = new SeqReader(seq, new SeqPosition(1, seq.headOption), eof)

  class SeqPosition[+T](val line: Int, element: Option[T]) extends scala.util.parsing.input.Position {
    val column = 1
    protected def lineContents = element match {
      case Some(e) => e.toString()
      case None => ""
    }
    override def toString = line.toString
  }

  trait HasEof[+T] {
    val eofValue: T
  }
}