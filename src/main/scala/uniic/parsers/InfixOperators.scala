package uniic.parsers

private[parsers] trait InfixOperators extends CommonParsers {
  type InfixOperand

  /** Overridable set of infix operators, each having a precedence (higher binds more tightly) and an associativity. */
  val infixOperators: Map[String, (Int, Associativity)]

  protected def makeInfixExpression(left: InfixOperand, op: String, right: InfixOperand): InfixOperand

  protected def highestPrecedence: PP[InfixOperand]

  protected lazy val lowestPrecedence: PP[InfixOperand] = {
    val allInfixes = infixOperators.map { case (op, (prec, assoc)) => (op, prec, assoc) }.toSeq

    def makeInfixParser(precedence: Int, higherPrecedenceParser: => PP[InfixOperand]): PP[InfixOperand] = {
      def byAssoc(assoc: Associativity): Seq[String] = {
        allInfixes.filter { case (_, p, a) => p == precedence && a == assoc }.map(_._1).sorted
      }
      val leftOps = byAssoc(Associativity.Left)
      val rightOps = byAssoc(Associativity.Right)
      val noAssocOps = byAssoc(Associativity.NoAssoc)

      def mkOpParser(ops: Seq[String]): PP[String] = {
        withCheck(identOperator)(ops.contains(_))(_ + " is not a known operator")
      }

      val leftParser = {
        chainl1(higherPrecedenceParser, mkOpParser(leftOps) ^^ { op =>
          (left: InfixOperand, right: InfixOperand) => makeInfixExpression(left, op, right)
        })
      }
      val rightParser = {
        simplerChainr1(higherPrecedenceParser, mkOpParser(rightOps) ^^ { op =>
          (left: InfixOperand, right: InfixOperand) => makeInfixExpression(left, op, right)
        })
      }
      val noAssocParser = {
        higherPrecedenceParser ~ mkOpParser(noAssocOps) ~ higherPrecedenceParser ^^ {
          case left ~ op ~ right => makeInfixExpression(left, op, right)
        }
      }

      (leftParser ||| rightParser ||| noAssocParser) | higherPrecedenceParser
    }

    var middlePrecedences = new java.util.TreeMap[Int, PP[InfixOperand]]
    val allCustomPrecedencesInDescendingOrder = allInfixes.map(_._2).toSet.toSeq.sorted.reverse
    for (prec <- allCustomPrecedencesInDescendingOrder) {
      val ceilEntry = middlePrecedences.ceilingEntry(prec + 1)
      val higherPrecedenceParser = {
        if (ceilEntry != null) {
          ceilEntry.getValue()
        } else {
          highestPrecedence
        }
      }
      middlePrecedences.put(prec, makeInfixParser(prec, higherPrecedenceParser))
    }

    if (middlePrecedences.isEmpty) {
      highestPrecedence
    } else {
      middlePrecedences.firstEntry().getValue()
    }
  }

  lazy val identOperator: PP[String] = (
    acceptIf(tok =>
      tok.isInstanceOf[lexical.Kw] &&
        Lexer.isIdentOperator(tok.asInstanceOf[lexical.Kw].chars)
    )(tok => "Unexpected " + tok) ^^ { _.chars }
  )
}