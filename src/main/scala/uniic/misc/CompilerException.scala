package uniic.misc
import uniic.types.TypeUnification

/** Adds context information to an exception type.
  *
  * See also: [[ExceptionContexts]] */
class CompilerException(msg: String, cause: Throwable) extends Exception(msg, cause) {
  def this(msg: String) = this(msg, null)
  def this(cause: Throwable) = this(null, cause)
  private var contextLines = List.empty[String]

  def context: List[String] = contextLines.reverse

  def addContext(higherContext: String) {
    contextLines ::= higherContext
  }

  override def getMessage = {
    def causeLine = cause.getClass.getSimpleName + ": " + cause.getMessage
    var lines = (super.getMessage, cause) match {
      case (null, null) => context
      case (msg, null) => msg :: context
      case (null, cause) => causeLine :: context
      case (msg, cause) => msg :: context ++ List("", "Cause: " + causeLine)
    }
    lines mkString "\n"
  }
}

class TypeError(msg: String) extends CompilerException(msg)
class EscapeAnalysisError(msg: String) extends CompilerException(msg)
