package numbers

import numbers.model._

object EnglishNumbers {
  def toOnes(digit: Digit): String = digit.toString.toLowerCase

  def toTens(digit: Digit): String = digit match {
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

  def toTwoDigit(second: Digit, first: Digit): String = (second, first) match {
    case (One, One)   => "eleven"
    case (One, Two)   => "twelve"
    case (One, Three) => "thirteen"
    case (One, Four)  => "fourteen"
    case (One, Five)  => "fifteen"
    case (One, Six)   => "sixteen"
    case (One, Seven) => "seventeen"
    case (One, Eight) => "eighteen"
    case (One, Nine)  => "nineteen"
    case _ =>
      toTens(second) + " " + toOnes(first)
  }

  private def englishNumberPart(x: Digit, y: Digit, z: Digit): String = {
    lazy val hundreds = toOnes(x) + " hundred"

    lazy val tens = toTens(y)

    lazy val ones = toOnes(z)

    lazy val twoDigitNumber = toTwoDigit(y, z)

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

  def convert(num: IntegerModel): Option[String] = {
    val sign = if (num.isNegative) "minus " else ""

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

    val labelled = labels
      .foldLeft[(List[Option[String]], List[Digit])]( ( Nil, num.digits.dropRight(3) ) )(
        (accum, label) => accum match {
          case (strs, ds) =>
            ( threeDigitPattern(ds, label) :: strs
            , ds.dropRight(3)
            )
        })
      ._1
      .filter(_.nonEmpty)
      .map(_.get)
      .reduceOption(_ + ", " + _)
      .getOrElse("")


    threeDigitPattern(num.digits, "").map(str =>
      sign +
        ((str == "zero", labelled.isEmpty) match {
           case (true, false) =>
             labelled
           case (false, false) =>
             labelled + ", " + str
           case _ =>
             str
        })
    )
  }
}
