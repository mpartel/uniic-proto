package uniic.misc

//FIXME: unit tests missing. must write unit tests.

/**
 * API for marking blocks of code for tracing
 * and exception annotation.
 *
 *  Usage:
 *  {{{
 *  context("checking foobars") {
 *    // ...
 *  }
 *  }}}
 *
 *  You should label contexts so that the word "while" looks natural in front of them.
 */
trait Contexts {
  import Contexts._

  protected def context[T](level: ContextLevel, desc: => String)(block: => T): T = block
  protected final def context[T](desc: => String)(block: => T): T = context(DefaultLevel, desc)(block)

  protected final def verboseContext[T](desc: => String)(block: => T): T = context(VerboseLevel, desc)(block)

  protected def comment(level: ContextLevel, msg: => String) {}
  protected final def comment(msg: => String) {
    comment(DefaultLevel, msg)
  }
  protected final def verboseComment(msg: => String) {
    comment(VerboseLevel, msg)
  }
}

object Contexts {
  abstract class ContextLevel(val verbosity: Int) extends Ordered[ContextLevel] {
    def compare(that: ContextLevel) = this.verbosity - that.verbosity
  }
  case object OffLevel extends ContextLevel(-1)
  case object DefaultLevel extends ContextLevel(0)
  case object VerboseLevel extends ContextLevel(1)
}


/** Adds to a thrown [[CompilerException]] the contexts it passes through. */
trait CompilerExceptionContexts extends Contexts {
  override protected def context[T](level: Contexts.ContextLevel, desc: => String)(block: => T): T = {
    val forcedDesc = desc
    try {
      super.context(level, forcedDesc)(block)
    } catch {
      case ex: CompilerException => throw addContextToCompilerException(level, forcedDesc, ex)
    }
  }

  protected def addContextToCompilerException[T <: CompilerException](level: Contexts.ContextLevel, context: String, ex: T): T = {
    ex.addContext("while " + context)
    ex
  }
}

/** Like [[CompilerExceptionContexts]] but wraps any exception into a CompilerException as it first passes through a context */
trait ExceptionContexts extends CompilerExceptionContexts {
  override protected def context[T](level: Contexts.ContextLevel, desc: => String)(block: => T): T = {
    val forcedDesc = desc
    try {
      super.context(level, forcedDesc)(block)
    } catch {
      case ex: CompilerException => throw ex // Was handled by superclass
      case ex: Throwable => {
        val wrapper = new CompilerException(ex)
        addContextToCompilerException(level, desc, wrapper)
        throw wrapper
      }
    }
  }
}


/** Include this temporarily to get debug prints for when contexts are entered and exited. */
trait DebugContexts extends Contexts {
  def logLevel: Contexts.ContextLevel = Contexts.DefaultLevel

  override protected def context[T](level: Contexts.ContextLevel, desc: => String)(block: => T): T = {
    val indents = debugContextIndents
    debugContextPrint(Contexts.DefaultLevel, indents + "Starting " + desc)
    DebugContexts.curDepth += 1
    try {
      super.context(level, desc)(block)
    } finally {
      DebugContexts.curDepth -= 1
      debugContextPrint(Contexts.DefaultLevel, indents + "Finished " + desc)
    }
  }

  protected def debugContextIndents = ("  " * DebugContexts.curDepth)

  protected override def comment(level: Contexts.ContextLevel, msg: => String) {
    super.comment(level, msg)
    def indents = debugContextIndents
    def indentedMsg = msg.lines.map(indents + _).mkString("\n")
    debugContextPrint(level, indentedMsg)
  }

  protected def debugContextPrint(level: Contexts.ContextLevel, s: => String) = {
    if (level <= this.logLevel) {
      println(s)
    }
  }
}

object DebugContexts {
  private var thisThreadCurDepth: InheritableThreadLocal[Int] = new InheritableThreadLocal[Int]() {
    override protected def initialValue = 0
  }

  def curDepth = thisThreadCurDepth.get
  def curDepth_=(newDepth: Int) { thisThreadCurDepth.set(newDepth) }
}
