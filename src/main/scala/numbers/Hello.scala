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

object NumberConverter {
  def englishOnes(digit: Digit): String = digit.toString.toLowerCase

  def englishTens(digit: Digit): String = digit match {
    case Zero  => ""
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

  def englishNumber(num: Integer): Option[String] = {
    val sign = if (num.isNegative) "minus " else ""

    def englishNumberPart(x: Digit, y: Digit, z: Digit): String = {
      lazy val hundreds = englishOnes(x) + " hundred"

      lazy val tens = englishTens(y)

      lazy val ones = englishOnes(z)

      lazy val twoDigitNumber = (y, z) match {
        case (One, One) => "eleven"
        case (One, Two) => "twelve"
        case (One, Three) => "thirteen"
        case (One, Four) => "fourteen"
        case (One, Five) => "fifteen"
        case (One, Six) => "sixteen"
        case (One, Seven) => "seventeen"
        case (One, Eight) => "eighteen"
        case (One, Nine) => "nineteen"
        case _ =>
          tens + " " + ones
      }

      (x, y, z) match {
        case (Zero, Zero, z) =>
          ones
        case (Zero, y, Zero) =>
          tens
        case (x, Zero, Zero) =>
          hundreds
        case (Zero, y, z) =>
          twoDigitNumber
        case (x, Zero, z) =>
          hundreds + " and " + ones
        case (x, y, Zero) =>
          hundreds + " and " + tens
        case (x, y, z) =>
          hundreds + " and " + twoDigitNumber
      }
    }

    def threeDigitPattern(digits: List[Digit], label: String): Option[String] = {
      val number = digits.takeRight(3) match {
        case List(z) =>
          Some(englishNumberPart(Zero, Zero, z))
        case List(y, z) =>
          Some(englishNumberPart(Zero, y, z))
        case List(x, y, z) =>
          Some(englishNumberPart(x, y, z))
        case _ =>
          None
      }

      number.map(_ + label)
    }

    val labels = List(" thousand", " million")

    val labelled =
      labels.foldLeft[(List[Option[String]], List[Digit])]( (Nil, num.digits.dropRight(3)) )(
        (accum, label) => accum match {
          case (strs, ds) => (threeDigitPattern(ds, label) :: strs, ds.dropRight(3))
        })._1.filter(_.nonEmpty).map(_.get).reduceOption(_ + ", " + _).getOrElse("")


    threeDigitPattern(num.digits, "")
      .map(str => sign + ((str == "zero", labelled.isEmpty) match {
                            case (true, false) =>
                              labelled
                            case (false, false) =>
                              labelled + ", " + str
                            case _ =>
                              str
                          }))
  }
}

object Hello extends Greeting with App {

  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
