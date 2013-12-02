package uniic.types
import org.scalatest.matchers._

trait TypeSchemaAssertions {
  def beInstantiatableFrom(right: TypeSchema) = {
    new Matcher[TypeSchema] {
      def apply(left: TypeSchema) = {
        MatchResult(
          left.canBeInstantiatedFrom(right),
          left + " was not an instance of " + right,
          left + " was an instance of  " + right
        )
      }
    }
  }

  def beEquivalentTo(right: TypeSchema) = {
    new Matcher[TypeSchema] {
      def apply(left: TypeSchema) = {
        MatchResult(
          left.isEquivalentTo(right),
          left + " was not equivalent to " + right,
          left + " was equivalent to  " + right
        )
      }
    }
  }
}