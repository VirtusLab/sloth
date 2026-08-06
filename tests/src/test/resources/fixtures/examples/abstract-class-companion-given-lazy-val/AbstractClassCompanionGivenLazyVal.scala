// Reproduces scala-cli DirectiveValueParser pattern:
// Abstract class with companion object containing lazy given instances.
// Inner class in companion extends the abstract class.
// When companion object loads, JVM must load the abstract class first
// (because inner class extends it), creating a class loading ordering
// issue for the agent's companion buffering.

abstract class Parser[+T]:
  def parse(input: String): T

  final def map[U](f: T => U): Parser[U] =
    new Parser.Mapped[T, U](this, f)

object Parser:
  private final class Mapped[T, +U](underlying: Parser[T], f: T => U) extends Parser[U]:
    def parse(input: String): U =
      f(underlying.parse(input))

  given Parser[String] = new Parser[String]:
    def parse(input: String): String = input

  given Parser[Int] = new Parser[Int]:
    def parse(input: String): Int = input.toInt

  given Parser[Boolean] = new Parser[Boolean]:
    def parse(input: String): Boolean = input.toBoolean

@main def main() =
  val sp = summon[Parser[String]]
  val ip = summon[Parser[Int]]
  val bp = summon[Parser[Boolean]]
  val mapped = sp.map(_.length)
  println(s"${sp.parse("hello")},${ip.parse("42")},${bp.parse("true")},${mapped.parse("test")}")
