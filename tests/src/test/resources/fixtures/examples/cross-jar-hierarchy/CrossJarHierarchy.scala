package slothtest.crossjar

abstract class HierBase:
  def label: String

final class HierSubA extends HierBase:
  def label: String = "A"

final class HierSubB extends HierBase:
  def label: String = "B"

object CrossJarHolder:

  // Opaque to the compiler, so the branch below survives as a real stack-map merge point
  // instead of being folded away.
  private def preferA: Boolean = System.nanoTime() > 0L

  def choose(): HierBase =
    val chosen: HierBase = if preferA then new HierSubA else new HierSubB
    chosen

  lazy val picked: String =
    val chosen: HierBase = if preferA then new HierSubA else new HierSubB
    chosen.label

@main def main(): Unit =
  println(s"${CrossJarHolder.choose().label}${CrossJarHolder.picked}")
