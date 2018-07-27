package numbers.model

import org.scalatest._

class DigitSpec extends FunSpec with Matchers {
  describe("Digit") {
    import Digit._

    describe("parse") {
      describe("if the given character is a digit") {
        it("returns the digit") {
          parse('0') shouldEqual Some(Zero)
          parse('1') shouldEqual Some(One)
          parse('2') shouldEqual Some(Two)
          parse('3') shouldEqual Some(Three)
          parse('4') shouldEqual Some(Four)
          parse('5') shouldEqual Some(Five)
          parse('6') shouldEqual Some(Six)
          parse('7') shouldEqual Some(Seven)
          parse('8') shouldEqual Some(Eight)
          parse('9') shouldEqual Some(Nine)
        }
      }
      describe("otherwise") {
        it("returns None") {
          parse('?') shouldEqual None
          parse('-') shouldEqual None
          parse('a') shouldEqual None
          parse(' ') shouldEqual None
        }
      }
    }
  }
}
