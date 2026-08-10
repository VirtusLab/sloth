object LazyValException:
  var attempts = 0
  lazy val failing: Int =
    attempts += 1
    throw new RuntimeException("oops")
  lazy val safe: String = "safe"

@main def main(): Unit =
  println(s"safe = ${LazyValException.safe}")
  try println(LazyValException.failing)
  catch case e: RuntimeException => println(s"caught: ${e.getMessage}")
  try println(LazyValException.failing)
  catch case e: RuntimeException => println(s"caught again: ${e.getMessage}")
  println(s"attempts = ${LazyValException.attempts}")
