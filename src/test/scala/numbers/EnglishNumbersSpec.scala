package numbers

import org.scalatest._

import numbers.model._

class EnglishNumbersSpec extends FunSpec with Matchers {
  describe("EnglishNumbers") {
    import EnglishNumbers._

    describe("toOnes") {
      it("return the English word for the given digit") {
        toOnes(Zero)  shouldEqual "zero"
        toOnes(One)   shouldEqual "one"
        toOnes(Two)   shouldEqual "two"
        toOnes(Three) shouldEqual "three"
        toOnes(Four)  shouldEqual "four"
        toOnes(Five)  shouldEqual "five"
        toOnes(Six)   shouldEqual "six"
        toOnes(Seven) shouldEqual "seven"
        toOnes(Eight) shouldEqual "eight"
        toOnes(Nine)  shouldEqual "nine"
      }
    }

    describe("toTens") {
      describe("if the given number isn't zero") {
        it("return the English word for the given digit") {
          toTens(One)   shouldEqual "ten"
          toTens(Two)   shouldEqual "twenty"
          toTens(Three) shouldEqual "thirty"
          toTens(Four)  shouldEqual "fourty"
          toTens(Five)  shouldEqual "fifty"
          toTens(Six)   shouldEqual "sixty"
          toTens(Seven) shouldEqual "seventy"
          toTens(Eight) shouldEqual "eighty"
          toTens(Nine)  shouldEqual "ninety"
        }
      }
      describe("otherwise") {
        it("returns an empty string") {
          toTens(Zero) shouldEqual ""
        }
      }
    }



    describe("convert") {
      it("returns the number in English words") {
        convert(IntegerModel(true, List(One)))   shouldEqual Some("minus one")
        convert(IntegerModel(false, List(Zero))) shouldEqual Some("zero")

        convert(IntegerModel(false, List(Two, One)))   shouldEqual Some("twenty one")
        convert(IntegerModel(false, List(Nine, Zero))) shouldEqual Some("ninety")

        convert(IntegerModel(false, List(One, Zero, Five)))   shouldEqual Some("one hundred and five")
        convert(IntegerModel(false, List(Five, One, Zero)))   shouldEqual Some("five hundred and ten")
        convert(IntegerModel(true,  List(Seven, Zero, Zero))) shouldEqual Some("minus seven hundred")

        convert(IntegerModel(false, List(Eight, Zero, Zero, Zero))) shouldEqual
          Some("eight thousand")
        convert(IntegerModel(false, List(Eight, Zero, Seven, Zero))) shouldEqual
          Some("eight thousand, seventy")
        convert(IntegerModel(true, List(One, Two, Three, Four, Five))) shouldEqual
          Some("minus twelve thousand, three hundred and fourty five")

        convert(IntegerModel(false, List(Five, Six, Nine, Four, Five, Seven, Eight, One))) shouldEqual
          Some("fifty six million, nine hundred and fourty five thousand, seven hundred and eighty one")
      }
    }
  }
}
