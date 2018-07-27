package numbers.model

case class IntegerModel(isNegative: Boolean, digits: List[Digit])

object IntegerModel extends Parser[String, IntegerModel] {
  def parse(string: String): Option[IntegerModel] = {
    def parseDigits(str: String): Option[List[Digit]] =
      if (str.isEmpty) Some(Nil)
      else Digit.parse(str(0)).flatMap(d => parseDigits(str.tail).map(d :: _))

    def notEmpty[A](list: List[A]): Option[List[A]] =
      if (list.isEmpty) None else Some(list)

    if (string.isEmpty) None
    else if (string(0) == '-')
      parseDigits(string.tail).flatMap(notEmpty).map(IntegerModel(true, _))
    else parseDigits(string).flatMap(notEmpty).map(IntegerModel(false, _))
  }
}
