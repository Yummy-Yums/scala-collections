//> using test.dep org.scalameta::munit::0.7.29
//> using file Option.scala

import munit.FunSuite
import Option._

class OptionTest extends FunSuite {

  test("map option of one type to another type"){

    assertEquals(map(Some(3))(_.toDouble), Some(3.0))

    test("test for None"){
      assertEquals(map(None)(_.toString), None)
    }
 
  }

  test("test for flatMap"){
    assertEquals(flatMap[Int, Int](Some(42), x => Some(x * 2)), Some(84))
  }
  
  test("map2 should combine two Options using a function"){
    val optionA: Option[Int] = Some(5)
    val optionB: Option[Int] = Some(7)

    val result: Option[Int]  = map2(optionA, optionB)(_ + _)
    val result2 = map2(optionA, None)((a: Int, b: Int) => a + b) 

    assertEquals(result, Some(12))
    assertEquals(result2, None)
  }
  
  test("get a value or return a default"){

    assertEquals(getOrElse(Some(3), 0), 3)
    assertEquals(getOrElse(None, 0), 0)

  }

  test("orElse: returns Original if not None or return provided Option as an alternative"){  

    assertEquals(orElse(Some(2), Some(15)), Some(2))
    assertEquals(orElse(None, Some(15)), Some(15))
 
  }

  test("filter on Option type"){
    assertEquals(filter(Some(3))(x => x > 12), None)
    assertEquals(filter(Some(26))(x => x > 12), Some(26))
  }
  
  test("test sequence "){
    assertEquals(sequence(List(Some(1), Some(2), Some(3))), Some(List(1,2,3)))
  }

  test("test traverse"){
    import scala.util.{Try,Success}
    val list1 = List("1", "2", "3")
    val list2 = List("I", "II", "III", "IV")

    def parseInt(a: String): Option[Int] = 
      Try(a.toInt) match {
        case Success(r) => Some(r)
        case _ => None
      }

    assertEquals(traverse(list1)(i => parseInt(i)), Some(List(1,2,3)))
    assertEquals(traverse(list2)(i => parseInt(i)), None)
  }
}

