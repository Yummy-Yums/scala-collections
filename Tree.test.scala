//> using test.dep org.scalameta::munit::0.7.29
//> using file Tree.scala

import munit.FunSuite
import BinaryTree._

class TreeTests extends munit.FunSuite {
  
  test("empty tree"){ 
    val e = BinaryTree()
    assertEquals(e, EmptyTree)
    assertEquals(BinaryTree.empty, EmptyTree)
  }

  test("create a Tree"){

    val e = create(2, BinaryTree(1), BinaryTree(3))
    
    val expected = TreeNode(2,TreeNode(1,EmptyTree,EmptyTree),TreeNode(3,EmptyTree,EmptyTree))
    assertEquals(e, expected)

  }

  test("size of a Tree"){

    val e = BinaryTree(2, BinaryTree(1), BinaryTree(3))
     
    assertEquals(size(e), 3)
  }

  test("check whether a tree contains a value"){
    val e = BinaryTree(2, BinaryTree(1), BinaryTree(3))

    assertEquals(contains(e, 2), true)

  }

  test("depth of tree"){
    val e = BinaryTree.create(2, BinaryTree(1), BinaryTree(3))
    assertEquals(depth(e), 2)

  }

  test("transform elements of a tree using map"){
   val e = BinaryTree.create(2, BinaryTree(1), BinaryTree(3))
   val expected = TreeNode(4,TreeNode(2,EmptyTree,EmptyTree),TreeNode(6,EmptyTree,EmptyTree))

   assertEquals(map(e)(_ * 2), expected)
  }

  test("fold should correctly accumulate values in a binary tree"){
    val tree1 = TreeNode(1, TreeNode(2, EmptyTree, EmptyTree), TreeNode(3, EmptyTree, EmptyTree))
    val tree2 = TreeNode(4, TreeNode(5, TreeNode(6, EmptyTree, EmptyTree), EmptyTree), TreeNode(7, EmptyTree, EmptyTree))

    val sumFunction: (Int, Int, Int) => Int = (a, b, c) => a + b + c

    assertEquals(BinaryTree.fold(tree1)(0)(sumFunction), 6)
    assertEquals(BinaryTree.fold(tree2)(0)(sumFunction), 22)

  }

  test("insert data into tree"){
    val tree1 = TreeNode(1, TreeNode(2, EmptyTree, EmptyTree), TreeNode(3, EmptyTree, EmptyTree))
    val e = insert(tree1, Some(5))
    val expected = TreeNode(5,TreeNode(1,TreeNode(2,EmptyTree,EmptyTree),TreeNode(3,EmptyTree,EmptyTree)),EmptyTree)

    assertEquals(e, expected)

  }

  val treeTest: BinaryTree[Int] =
    TreeNode(
      27,
      TreeNode(
        14,
        TreeNode(5, EmptyTree, TreeNode(10, TreeNode(6, EmptyTree, TreeNode(9, EmptyTree, EmptyTree)), EmptyTree)),
        EmptyTree
      ),
      TreeNode(31, EmptyTree, EmptyTree)
    )

  test("in-order traversal of a tree"){
    val tree1 = TreeNode(1, TreeNode(2, EmptyTree, EmptyTree), TreeNode(3, EmptyTree, EmptyTree))
    val expected = TreeNode(5,TreeNode(1,TreeNode(2,EmptyTree,EmptyTree),TreeNode(3,EmptyTree,EmptyTree)),EmptyTree)
    
    println(inOrderTraversal(treeTest))

    assertEquals(inOrderTraversal(expected), List(2,1,3,5))
    assertEquals(inOrderTraversal(treeTest), List(5, 6, 9, 10, 14, 27, 31))

  }

  test("post-order traversal of traversal"){
   val expected = TreeNode(5,TreeNode(1,TreeNode(2,EmptyTree,EmptyTree),TreeNode(3,EmptyTree,EmptyTree)),EmptyTree)

   assertEquals(postOrderTraversal(expected), List(2, 3, 1, 5))
   println(postOrderTraversal(treeTest))
   assertEquals(postOrderTraversal(treeTest), List(9, 6, 10, 5, 14, 31, 27))


  }

  test("pre-order traversal of tree"){
    val expected = TreeNode(5,TreeNode(1,TreeNode(2,EmptyTree,EmptyTree),TreeNode(3,EmptyTree,EmptyTree)),EmptyTree)
    println(preOrderTraversal(treeTest))

    assertEquals(preOrderTraversal(expected), List(5, 1, 2, 3))
    assertEquals(preOrderTraversal(treeTest), List(27, 14, 5, 10, 6, 9, 31))

    
  }
}

