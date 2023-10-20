//> using test.dep org.scalameta::munit::0.7.29
//> using file Stream.scala


import Stream._

class StreamTests extends munit.FunSuite {
 
  test("Stream smart constructors"){
    assertEquals(toList(cons(1, Stream(2,3))), List(1,2,3))  
  }

  test("take a number of elements from a Stream"){
    assertEquals(toList(take(Stream(1, 2, 3), 2)), List(1, 2, 3))
  }

  test("drop a number of first n elements"){
    assertEquals(toList(drop(Stream(1,2,3,4,5), 2)), List(3, 4, 5))
  }

  test("takeWhile: return elements that match a given predicate"){
    
    assertEquals(toList(takeWhile(Stream(1,2,3,4,5), (x: Int) => x < 3)), List(1, 2))
    assertEquals(toList(takeWhile_1(Stream(1,2,3,4,5), (x: Int) => x < 3)), List(1, 2))


    assertEquals(toList(takeWhile(Stream(1,2,3,4,5), (x: Int) => x < 0)), List())
    assertEquals(toList(takeWhile_1(Stream(1,2,3,4,5), (x: Int) => x < 0)), List())


  }

  test("fold elements of a Stream to the right"){
    def sum(a: Int, b: => Int): Int = a + b

    assertEquals(foldRight(Stream(1,2,3,4,5))(0)(sum), 15)
  }

  test("check if all elements match a given predicate"){
    assertEquals(forAll(Stream(1,2,3,4,5), (x: Int) => x % 2 == 0), false)

  }

  test("return the head element wrapped in an Option"){
    assertEquals(headOption(Stream(1,2,3)), Some(1))
  }

  test("map a function on elements of a stream"){
    assertEquals(toList(map(Stream(1,2,3,4,5), (i: Int) => i.toString())), List("1", "2", "3", "4", "5"))
  }

  test("Filter elements out of a Stream"){
    assertEquals(toList(filter(Stream(1,2,3,4,5), (i: Int) => i > 3)), List(4, 5))
  }

  test("flatMap a Stream"){
    val inputStream: Stream[Int] = Stream(1, 2, 3, 4, 5)

    def duplicateAndIncrement(i: Int): Stream[Int] =
      Stream(i, i + 1)

    val resultStream: Stream[Int] = flatMap(inputStream,duplicateAndIncrement)

    val expectedResult: List[Int] = List(1, 2, 2, 3, 3, 4, 4, 5, 5, 6)

    assertEquals(toList(resultStream), expectedResult)
  }

  test("append 2 streams"){
    assertEquals(toList(append(Stream(1,2,3), Stream(4, 5))), List(1, 2, 3, 4, 5))
  }

}

