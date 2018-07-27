package numbers

import numbers.model._

import scala.io.StdIn.readLine

object Application extends App {
  print("Enter an integer: ")

  IntegerModel.parse(readLine).flatMap(EnglishNumbers.convert _) match {
    case Some(str) =>
      println(str)
    case None =>
      println("Invalid input!")
  }
}
