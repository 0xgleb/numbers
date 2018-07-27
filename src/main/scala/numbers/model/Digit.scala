package numbers.model

sealed trait Digit
case object Zero  extends Digit
case object One   extends Digit
case object Two   extends Digit
case object Three extends Digit
case object Four  extends Digit
case object Five  extends Digit
case object Six   extends Digit
case object Seven extends Digit
case object Eight extends Digit
case object Nine  extends Digit

object Digit extends Parser[Char, Digit] {
  def parse(char: Char): Option[Digit] = char match {
    case '0' => Some(Zero)
    case '1' => Some(One)
    case '2' => Some(Two)
    case '3' => Some(Three)
    case '4' => Some(Four)
    case '5' => Some(Five)
    case '6' => Some(Six)
    case '7' => Some(Seven)
    case '8' => Some(Eight)
    case '9' => Some(Nine)
    case _   => None
  }
}
