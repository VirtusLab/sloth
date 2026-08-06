object MultipleLazyVals:
  lazy val first: String = "one"
  lazy val second: Int = 42
  lazy val third: Double = 3.14
  lazy val fourth: Boolean = true

@main def main() =
  println(
    s"${MultipleLazyVals.first}, ${MultipleLazyVals.second}, ${MultipleLazyVals.third}, ${MultipleLazyVals.fourth}"
  )
