object ComplexInit:
  lazy val computed: Int =
    val x = 10
    val y = 20
    val factors = List(1, 2, 3)
    factors.map(_ * x).sum + y

  lazy val multiStep: String =
    val builder = StringBuilder()
    for i <- 1 to 3 do builder.append(s"step$i ")
    builder.toString.trim

  lazy val withPatternMatch: String =
    val input: Any = 42
    input match
      case i: Int if i > 0 => s"positive-$i"
      case i: Int          => s"non-positive-$i"
      case _               => "unknown"

@main def main(): Unit =
  println(s"computed = ${ComplexInit.computed}")
  println(s"multiStep = ${ComplexInit.multiStep}")
  println(s"withPatternMatch = ${ComplexInit.withPatternMatch}")
