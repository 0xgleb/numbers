package numbers

import org.scalatest._

class HelloSpec extends FunSpec with Matchers {
  describe("NumberConverter") {
    import NumberConverter._
    describe("englishOnes") {
      it("return the English word for the given digit") {
        englishOnes(Zero)  shouldEqual "zero"
        englishOnes(One)   shouldEqual "one"
        englishOnes(Two)   shouldEqual "two"
        englishOnes(Three) shouldEqual "three"
        englishOnes(Four)  shouldEqual "four"
        englishOnes(Five)  shouldEqual "five"
        englishOnes(Six)   shouldEqual "six"
        englishOnes(Seven) shouldEqual "seven"
        englishOnes(Eight) shouldEqual "eight"
        englishOnes(Nine)  shouldEqual "nine"
      }
    }

    describe("englishTens") {
      it("return the English word for the given digit") {
        englishTens(Zero)  shouldEqual "zero"
        englishTens(One)   shouldEqual "ten"
        englishTens(Two)   shouldEqual "twenty"
        englishTens(Three) shouldEqual "thirty"
        englishTens(Four)  shouldEqual "fourty"
        englishTens(Five)  shouldEqual "fifty"
        englishTens(Six)   shouldEqual "sixty"
        englishTens(Seven) shouldEqual "seventy"
        englishTens(Eight) shouldEqual "eighty"
        englishTens(Nine)  shouldEqual "ninety"
      }
    }

    describe("getDigit") {
      describe("if the given character is a digit") {
        it("returns the digit") {
          getDigit('0') shouldEqual Some(Zero)
          getDigit('1') shouldEqual Some(One)
          getDigit('2') shouldEqual Some(Two)
          getDigit('3') shouldEqual Some(Three)
          getDigit('4') shouldEqual Some(Four)
          getDigit('5') shouldEqual Some(Five)
          getDigit('6') shouldEqual Some(Six)
          getDigit('7') shouldEqual Some(Seven)
          getDigit('8') shouldEqual Some(Eight)
          getDigit('9') shouldEqual Some(Nine)
        }
      }
      describe("otherwise") {
        it("returns None") {
          getDigit('?') shouldEqual None
          getDigit('-') shouldEqual None
          getDigit('a') shouldEqual None
          getDigit(' ') shouldEqual None
        }
      }
    }

    describe("parseString") {
      describe("if the given string represents an integer") {
        it("returns the integer") {
          parseString("0")   shouldEqual Some(Integer(false, List(Zero)))
          parseString("123") shouldEqual Some(Integer(false, List(One, Two, Three)))
          parseString("-49") shouldEqual Some(Integer(true, List(Four, Nine)))
        }
      }
      describe("otherwise") {
        it("returns None") {
          parseString("test") shouldEqual None
          parseString("-1??") shouldEqual None
          parseString("")     shouldEqual None
          parseString("-")    shouldEqual None
          parseString("123a") shouldEqual None
        }
      }
    }
  }
}
