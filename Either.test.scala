//> using test.dep org.scalameta::munit::0.7.29
//> using file Either.scala

import munit.FunSuite
import Either._


class EitherTests extends FunSuite {

  test("map - Right") {
    val rightValue: Either[String, Int] = Right(5)
    val result = map(rightValue, (x: Int) => x * 2)
    assert(result == Right(10))
  }

  test("map - Left") {
    val leftValue: Either[String, Int] = Left("Error")
    val result = map(leftValue, (x: Int) => x * 2)
    assert(result == Left("Error"))
  }

  test("orElse - Right") {
    val rightValue: Either[String, Int] = Right(5)
    val fallbackValue: Either[String, Int] = Right(10)
    val result = orElse(rightValue, fallbackValue)
    assert(result == Right(5))
  }

  test("orElse - Left") {
    val leftValue: Either[String, Int] = Left("Error")
    val fallbackValue: Either[String, Int] = Right(10)
    val result = orElse(leftValue, fallbackValue)
    assert(result == Right(10))
  }

  test("flatMap - Right") {
    val rightValue: Either[String, Int] = Right(5)
    val result = flatMap(rightValue, (x: Int) => Right(x * 2))
    assert(result == Right(10))
  }

  test("flatMap - Left") {
    val leftValue: Either[String, Int] = Left("Error")
    val result = flatMap(leftValue, (x: Int) => Right(x * 2))
    assert(result == Left("Error"))
  }

  test("map2 - Right") {
    val eitherA: Either[String, Int] = Right(5)
    val eitherB: Either[String, Double] = Right(3.0)
    val result = map2(eitherA, eitherB)((a: Int, b: Double) => a + b)
    assert(result == Right(8.0))
  }

  test("map2 - Left") {
    val eitherA: Either[String, Int] = Left("Error")
    val eitherB: Either[String, Double] = Right(3.0)
    val result = map2(eitherA, eitherB)((a: Int, b: Double) => a + b)
    assert(result == Left("Error"))
  }

   test("traverse - All Right values") {
    val inputList: List[Int] = List(1, 2, 3, 4)
    val result = traverse(inputList)(x => Right(x * 2))
    assert(result == Right(List(2, 4, 6, 8)))
  }

  test("traverse - Some Left values") {
    val inputList: List[Int] = List(1, 2, 3, 4)
    val result = traverse(inputList) {
      case x if x % 2 == 0 => Right(x * 2)
      case _ => Left("Odd value")
    }
    assert(result == Left("Odd value"))
  }

  test("sequence - All Right values") {
    val inputList: List[Either[String, Int]] = List(Right(1), Right(2), Right(3), Right(4))
    val result = sequence(inputList)
    assert(result == Right(List(1, 2, 3, 4)))
  }

  test("sequence - Some Left values") {
    val inputList: List[Either[String, Int]] = List(Right(1), Left("Error"), Right(3), Left("Another Error"))
    val result = sequence(inputList)
    assert(result == Left("Error"))
  }
}

