package uniic.boolalg
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

/** Implements boolean minimization using a heuristic version of the
  * Quine-McCluskey algorithm as presented here
  * http://en.literateprograms.org/Quine-McCluskey_algorithm_(Java)
  * (on 28.09.2011) */
object BoolMinimizer {
  import BoolDnf.DontCare

  private type Clause = Array[Byte]

  private def trace(msg: => String) {
    //println(msg)
  }

  def apply(original: BoolTerm): BoolTerm = {
    if (original.size == 1) {
      return original
    }

    trace("Simplifying: " + original)
    trace("Converting to DNF (size = " + original.size + ", depth = " + original.depth + ", vars = " + original.variables.size + ")")
    BoolDnf(original) match {
      case Some(input) => {
        val clauseCount = input.clauses.size
        val avgClauseSize = if (input.clauses.size > 0) input.clauses.map(_.length).sum / input.clauses.size else 0
        trace("DNF clause count = " + clauseCount + ", avg clause size = " + avgClauseSize)

        val result = this(input).toBoolTerm

        trace("Result: " + result)
        result
      }
      case None => BTrue
    }
  }

  def apply(original: BoolDnf): BoolDnf = {
    if (original.clauses.isEmpty) {
      return original
    }

    trace("Finding prime implicants")
    val primeImplicants = new PrimeImplicantFinder(original).primeImplicants.toIndexedSeq

    trace("Building implication table")
    val implicationTable = makeImplicationTable(primeImplicants, original.clauses)

    trace("Extracing implicants")
    val optimizedClauses = heuristicallyExtractImplicantsWhilePossible(primeImplicants, implicationTable)

    trace("Removing unused vars")
    val (finalVars, finalClauses) = removeUnusedVars(original.vars, optimizedClauses)

    trace("Done. Reduced from " + original.clauses.size + " to " + finalClauses.size + " clauses")
    new BoolDnf(finalVars, finalClauses)
  }

  /**
   * Makes a table of (i, j) => whether implicants(i) implies originals(j)
   */
  private def makeImplicationTable(implicants: IndexedSeq[Clause], originals: IndexedSeq[Clause]): Array[Array[Boolean]] = {
    val il = implicants.length
    val ol = originals.length
    val result = (0 until il*ol).par.map {n =>
      val i = n / ol
      val j = n % ol
      implies(implicants(i), originals(j))
    }.toIndexedSeq

    Array.tabulate(implicants.length, originals.length) { (i, j) => result(i * ol + j) }
  }

  @inline private def implies(c1: Clause, c2: Clause): Boolean = {
    (0 until c1.length).forall(i => c1(i) == DontCare || c1(i) == c2(i))
  }

  /** Finds all terms implied by only one implicant.
    * Such an implicant is called essential.
    * The terms it implies are then removed by marking them as not implied by anything.
    */
  private def extractEssentialImplicant(implTable: Array[Array[Boolean]]): Option[Int] = {
    for (j <- implTable(0).indices) {
      var implIndex: Option[Int] = None
      breakable {
        for (i <- implTable.indices) {
          if (implTable(i)(j)) {
            implIndex match {
              case None => implIndex = Some(i)
              case Some(_) => implIndex = None; break;
            }
          }
        }
      }

      if (implIndex.isDefined) {
        removeTermsImpliedBy(implTable, implIndex.get) // (could optimize away outer loop here)
        return implIndex
      }
    }
    return None
  }

  /** Removes all terms implied by `implIndex`.
    * A term is removed by marking it not implied by anything in the implication table. */
  private def removeTermsImpliedBy(implTable: Array[Array[Boolean]], implIndex: Int) {
    for (j <- implTable(0).indices) {
      if (implTable(implIndex)(j)) {
        for (i <- implTable.indices) {
          implTable(i)(j) = false
        }
      }
    }
  }

  /** Finds the implicant with the largest amount of terms it implies.
    * The heuristic is to greedily extract the largest implicants for as long as possible. */
  private def extractLargestImplicant(implTable: Array[Array[Boolean]]): Option[Int] = {
    var best = 0
    var bestIndex: Option[Int] = None
    for (i <- implTable.indices) {
      val count = implTable(i).count(_ == true)
      if (count > best) {
        best = count
        bestIndex = Some(i)
      }
    }

    bestIndex match {
      case Some(i) => removeTermsImpliedBy(implTable, i); Some(i)
      case None => None
    }
  }

  private def heuristicallyExtractImplicantsWhilePossible(clauses: Seq[Clause], implTable: Array[Array[Boolean]]): Array[Clause] = {
    var result = new ListBuffer[Clause]
    var done = false
    while (!done) {
      extractEssentialImplicant(implTable) match {
        case Some(i) => result += clauses(i)
        case None => {
          extractLargestImplicant(implTable) match {
            case Some(i) => result += clauses(i)
            case None => done = true
          }
        }
      }
    }
    result.toArray
  }

  private def removeUnusedVars(vars: Array[String], clauses: Array[Array[Byte]]): (Array[String], Array[Array[Byte]]) = {
    val usedVarIndices = vars.indices.filter(i => clauses.exists(_(i) != DontCare))
    val newVars = usedVarIndices.map(vars(_)).toArray
    val newClauses = new Array[Array[Byte]](clauses.length)
    for (i <- clauses.indices) {
      newClauses(i) = new Array[Byte](newVars.length)
      var newJ = 0
      for (oldJ <- usedVarIndices) {
        newClauses(i)(newJ) = clauses(i)(oldJ)
        newJ += 1
      }
    }
    (newVars, newClauses)
  }

  private class PrimeImplicantFinder(expr: BoolDnf) {

    val primeImplicants = ListBuffer[Clause](expr.clauses: _*)
    resolveTabulated(tabulateByDontCaresAndOnes)

    /** Makes a 2-dimensional table mapping (`numDontCares`, `numOnes`) to
      * a list of clauses that have those properties. */
    private def tabulateByDontCaresAndOnes: Array[Array[ListBuffer[Clause]]] = {
      val slots = expr.vars.length + 1 // A clause can have [0..vars] DontCares or ones.
      var result = Array.fill[ListBuffer[Clause]](slots, slots)(ListBuffer.empty)
      for (clause <- expr.clauses) {
        val numDontCares = clause.count(_ == DontCare)
        val numOnes = clause.count(_ == 1)
        result(numDontCares)(numOnes) += clause
      }
      result
    }

    /** Consumes the table created above and manipulates the prime implicant list
      * by trying to resolve all possible clauses `(c1, c2)` where `c1` has the same
      * number of `DontKnow`s and one less `1` than `c2`. */
    private def resolveTabulated(table: Array[Array[ListBuffer[Clause]]]) {
      val numVars = expr.vars.length
      for (numDontCares <- 1 until numVars;
           numOnes <- 0 until numVars) { // Inner loop could be parallelized, with writes to primeImplicants queued/serialized
        val outList = table(numDontCares)(numOnes)
        val c1List = table(numDontCares - 1)(numOnes)
        val c2List = table(numDontCares - 1)(numOnes + 1)
        for (c1 <- c1List;
             c2 <- c2List) {
          resolve(c1, c2) match {
            case Some(combined) => {
              if (!outList.exists(_.deep.equals(combined.deep))) {
                outList += combined
              }
              primeImplicants --= List(c1, c2)
              primeImplicants += combined
            }
            case None =>
          }
        }
      }
    }

    /** Tries to apply the resolution rule `(T /\ X) \/ (T /\ !X) = T`
      * with the assumption that c1 and c2 have the same number of DontCares. */
    private def resolve(c1: Clause, c2: Clause): Option[Clause] = {
      var diffIndex = -1
      for (i <- (0 until c1.length)) {
        if (c1(i) != c2(i)) {
          if (diffIndex == -1) {
            diffIndex = i
          } else {
            return None // More than one difference
          }
        }
      }
      if (diffIndex != -1) {
        var result = c1.clone()
        result(diffIndex) = DontCare
        Some(result)
      } else {
        None
      }
    }
  }
}