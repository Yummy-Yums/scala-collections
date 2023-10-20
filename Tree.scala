import collection.mutable._

sealed trait BinaryTree[+A]
case object EmptyTree extends BinaryTree[Nothing]
case class TreeNode[A](value: A, left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object BinaryTree {
  def empty[A]: BinaryTree[A] = EmptyTree

  def create[A](value: A, left: BinaryTree[A] = EmptyTree, right: BinaryTree[A] = EmptyTree): BinaryTree[A] = 
    TreeNode(value, left, right)

  def isEmpty[A](tree: BinaryTree[A]): Boolean = tree == EmptyTree

  def size[A](tree: BinaryTree[A]): Int = tree match {
    case EmptyTree => 0
    case TreeNode(_, left, right) => 1 + size(left) + size(right)
  }

  def contains[A](tree: BinaryTree[A], value: A): Boolean = tree match {
    case EmptyTree => false
    case TreeNode(v, left, right) if v == value => true
    case TreeNode(_, left, right) => contains(left, value) || contains(right, value)
  }

  def depth[A](tree: BinaryTree[A]): Int = tree match {
    case EmptyTree => 0
    case TreeNode(_, l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: BinaryTree[A])(f: A => B): BinaryTree[B] = t match {
    
    case EmptyTree => EmptyTree
    case TreeNode(v, l, r) => TreeNode(f(v), map(l)(f), map(r)(f)) 
    
  }

  def fold[A, B](t: BinaryTree[A])(z: B)(f: (A, B, B) => B): B = t match {
    case EmptyTree => z
    case TreeNode(v, left, right) => 
      val leftResult = fold(left)(z)(f)
      val rightResult = fold(right)(z)(f)
      f(v, leftResult, rightResult)
  }

  def insert[A](tree: BinaryTree[A], data: Option[A]): BinaryTree[A] = (tree, data) match {
    case (EmptyTree, None) => EmptyTree
    case (TreeNode(v, l, r), None) => tree
    case (TreeNode(_, _, _), data) => TreeNode(data.get, tree, EmptyTree)
    case _ => tree
  }
  
  def preOrderTraversal[A](tree: BinaryTree[A]): List[A] = {
     def traverse(tree: BinaryTree[A], result: ArrayBuffer[A]): Unit = {
        tree match {
          case EmptyTree => () 
          case TreeNode(value, left, right) =>
            result += value
            traverse(left, result)
            traverse(right, result)
        }
     }

    val result = ArrayBuffer.empty[A]
    traverse(tree, result)
    result.toList
  }

  def postOrderTraversal[A](tree: BinaryTree[A]): List[A] = {
    def traverse(tree: BinaryTree[A], result: ArrayBuffer[A]): Unit = {
      tree match {
        case EmptyTree => ()
        case TreeNode(value, left, right) => 
          traverse(left, result)
          traverse(right, result)
          result += value
      }
    }

    val result = ArrayBuffer.empty[A]
    traverse(tree, result)
    result.toList
  }

  def inOrderTraversal[A](tree: BinaryTree[A]): List[A] = {
   def traverse(tree: BinaryTree[A], result: ArrayBuffer[A]): Unit = {
     tree match {
       case EmptyTree => ()
       case TreeNode(value, left, right) =>
         traverse(left, result)
         result += value
         traverse(right, result)
     }
   }
   val result = ArrayBuffer.empty[A]
   traverse(tree, result)
   result.toList

  }

  
  def apply[A](as: A*): BinaryTree[A] = 
    if (as.isEmpty) EmptyTree
    else TreeNode(as.head, apply(as.tail: _*), EmptyTree)
}
