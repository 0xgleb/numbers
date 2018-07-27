package numbers

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

case class Integer(isNegative: Boolean, digits: List[Digit])

// def englishTens(digit: Digit): String

object NumberConverter {
  def englishOnes(digit: Digit): String = digit.toString.toLowerCase

  def englishTens(digit: Digit): String = digit match {
    case Zero  => "zero"
    case One   => "ten"
    case Two   => "twenty"
    case Three => "thirty"
    case Four  => "fourty"
    case Five  => "fifty"
    case Six   => "sixty"
    case Seven => "seventy"
    case Eight => "eighty"
    case Nine  => "ninety"
  }

  def getDigit(char: Char): Option[Digit] = char match {
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

  def parseString(string: String): Option[Integer] = {
    def parseDigits(str: String): Option[List[Digit]] =
      if (str.isEmpty) Some(Nil)
      else getDigit(str(0)).flatMap(d => parseDigits(str.tail).map(d :: _))

    def notEmpty[A](list: List[A]): Option[List[A]] =
      if (list.isEmpty) None else Some(list)

    if (string.isEmpty) None
    else if (string(0) == '-')
         parseDigits(string.tail).flatMap(notEmpty).map(Integer(true, _))
    else parseDigits(string).flatMap(notEmpty).map(Integer(false, _))
  }
}

object Hello extends Greeting with App {

  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
