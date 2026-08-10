enum Color:
  case Red, Green, Blue
  lazy val hex: String = this match
    case Red   => "#FF0000"
    case Green => "#00FF00"
    case Blue  => "#0000FF"

@main def main() =
  println(Color.Red.hex)
