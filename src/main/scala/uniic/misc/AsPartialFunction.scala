package uniic.misc

object AsPartialFunction {
  /** Converts a function B => C to a partial function A => C where A :> B. */
  def apply[A, B <: A : Manifest, C](f: B => C): PartialFunction[A, C] = {
    new PartialFunction[A, C] {
      def isDefinedAt(x: A) = manifest[B].runtimeClass.isInstance(x)
      def apply(x: A) = {
        if (isDefinedAt(x)) {
          f(x.asInstanceOf[B])
        } else {
          throw new MatchError(x)
        }
      }
    }
  }
}