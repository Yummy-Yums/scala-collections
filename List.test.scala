//> using test.dep org.scalameta::munit::0.7.29
//> using file List.scala

import munit.FunSuite
import FList._

class ListTests extends munit.FunSuite {
    
  test("empty List"){
    val e = FList()
    assertEquals(e, Nil)
  }

  test("sum elements in a List"){
    val f = FList.sum(FList(1,2,3,4,5))
    assertEquals(15, f)
  }

  test("product of elements in a List"){
    val r = FList.product(FList(1.0,2.0,3.0,4.0,5.0))
    assertEquals(120.0, r)
  }

  test("get tail of list"){
    val a = FList.tail(FList(1,2,3))
    assertEquals(a, FList(2,3))
  }

  test("replace the head of a list with another element"){
    val e = FList.setHead(FList(1,2,3), 0)
    assertEquals(e, FList(0,2,3))
  }

  test("drop a number of elements"){
    val q = FList.drop(FList(1,2,3,4,5), 2)
    
    assertEquals(q, FList(3, 4, 5))
  }

  test("drop a number of elements that matches a condition"){
    val g = FList.dropWhile(FList(1, 2, 3, 4, 5))((x: Int) => x < 2)
     
    assertEquals(g, FList(2, 3, 4, 5))
  }

  test("get the init of a List"){
    val w = FList.init(FList(1, 2, 3))
    
    assertEquals(FList(1,2), w)
  }

  test("fold elements of List to Left"){
    def sum(l: FList[Int]) = foldLeft(l, 0)(_ + _)
    def product(l: FList[Double]) = foldLeft(l, 1.0)(_ * _)

    assertEquals(product(FList(1.0, 2.0, 3.0)), 6.0)
    assertEquals(15, sum(FList(1, 2, 3, 4, 5)))
  }

  test("fold elements of List to right"){
    def concat(l: FList[String]) = foldRight(l, "")(_ + _)
    println(concat(FList("c", "d", "e")))
    //assertEquals()
  }

  test("reverse"){
    assertEquals(reverse(FList(1,2,3,4,5)), FList(5,4,3,2,1))
  }

  test("append"){
    assertEquals(append(FList(1,2,3,4), FList(5,6,7)), FList(1,2,3,4,5,6,7)) 
  }

  test("concatenate a list of lists"){
    assertEquals(concat(FList(FList(1,2,3), FList(4,5,6), FList(7,8,9))), FList(1,2,3,4,5,6,7,8,9))
  }

  test("map a function over elements in a list"){
    assertEquals(map(FList(1,2,3,4,5))(x => x + 2), FList(3,4,5,6,7))
  }

  test("flatMap"){
    assertEquals(flatMap(FList(1,2,3))(i => FList(i, i)), FList(1, 1, 2, 2, 3, 3))
  }

  test("zip elements of 2 list into one"){
    assertEquals(zipWith(FList("a", "b", "c"), FList("A", "B", "C"))(_ + _), FList("aA", "bB", "cC"))
  }

  test("test if a list starts with another list"){
    val l1 = FList(1,2,3,4,5)
    val l2 = FList(1,2)
    val l3 = FList(2,3)
    
    assertEquals(startsWith(l1, l2), true)
    assertEquals(startsWith(l1, l3), false)

  }

  test("has subsequence "){
    val l1 = FList(2, 3, 4, 5)
    val l2 = FList(3,4)

    assertEquals(hasSubsequence(l1, l2), true)
    assertEquals(hasSubsequence(l1, Nil), true)
  }
}
