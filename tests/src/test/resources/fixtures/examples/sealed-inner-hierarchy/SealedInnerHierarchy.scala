// Reproduces the case-app ParserCompanion$Step$ pattern:
// A sealed abstract class nested inside another abstract class, where
// the inner companion object gets $lzy1-suffixed fields for companion
// objects of case classes. These are eagerly initialized in the
// constructor, NOT actual lazy vals, but their names match the $lzy
// pattern and can cause false positives in detection.
abstract class Container:
  sealed abstract class Step(val index: Int)
  object Step:
    case class First(value: String) extends Step(0)
    case class Second(value: String) extends Step(1)
    case class Third(value: String) extends Step(2)

  def process(step: Step): String = step match
    case Step.First(v)  => s"first: $v"
    case Step.Second(v) => s"second: $v"
    case Step.Third(v)  => s"third: $v"

class MyContainer extends Container

@main def main() =
  val c = new MyContainer
  println(c.process(c.Step.First("a")))
  println(c.process(c.Step.Second("b")))
  println(c.process(c.Step.Third("c")))
