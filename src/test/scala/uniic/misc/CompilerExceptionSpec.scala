package uniic.misc

import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers

class CompilerExceptionSpec extends WordSpec with ShouldMatchers {
  "A CompilerException" when {
    "given context" should {
      "include the context lines after the message" in {
        val e = new CompilerException("things went wrong")
        e.addContext("while doing a suboperation")
        e.addContext("while doing a general operation")

        e.getMessage should be (
          """|things went wrong
             |while doing a suboperation
             |while doing a general operation""".stripMargin.replaceAllLiterally("\r", ""))
      }
    }
    "given context and a cause" should {
      "include the context lines after the message and before the cause" in {
        val cause = new IllegalArgumentException("got n < 0")
        val e = new CompilerException("things went wrong", cause)
        e.addContext("while doing a suboperation")
        e.addContext("while doing a general operation")

        e.getMessage should be (
          """|things went wrong
             |while doing a suboperation
             |while doing a general operation
             |
             |Cause: IllegalArgumentException: got n < 0""".stripMargin.replaceAllLiterally("\r", ""))
      }
    }
  }
}