package numbers.model

import org.scalatest._

class IntegerModelSpec extends FunSpec with Matchers {
  describe("IntegerModel") {
    import IntegerModel._

    describe("parse") {
      describe("if the given string represents an integer") {
        it("returns the integer") {
          parse("0")   shouldEqual Some(IntegerModel(false, List(Zero)))
          parse("123") shouldEqual Some(IntegerModel(false, List(One, Two, Three)))
          parse("-49") shouldEqual Some(IntegerModel(true, List(Four, Nine)))
        }
      }
      describe("otherwise") {
        it("returns None") {
          parse("test") shouldEqual None
          parse("-1??") shouldEqual None
          parse("")     shouldEqual None
          parse("-")    shouldEqual None
          parse("123a") shouldEqual None
        }
      }
    }
  }
}
