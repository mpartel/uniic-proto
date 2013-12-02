package uniic.test

import org.scalatest.FreeSpec

trait NumberedTestCases {
  self: FreeSpec =>

  protected var nextTestCaseNumber = 1

  protected def numberedTestCases(testName: String) = new {
    def -(block: => Unit) = {
      nextTestCaseNumber = 1
      testName - block
    }
  }

  protected def testCase(block: => Unit) = {
    val n = nextTestCaseNumber
    nextTestCaseNumber += 1
    ("case " + n) in block
  }
}