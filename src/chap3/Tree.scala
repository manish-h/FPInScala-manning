package chap3

object TreeRunner extends App {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A] {
    override def toString = value.toString()
  }
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def toString = "{" + left + "}.{" + right + "}"
  }

  object Tree {

    // Ex 3.25
    def size[A](tree: Tree[A]): Int = tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    // Ex 3.26
    def maxInt(tree: Tree[Int]): Int = tree match {
      case Leaf(value)         => value
      case Branch(left, right) => maxInt(left) max maxInt(right)
    }

    // Ex 3.27
    def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + depth(left) max depth(right)
    }

    // Ex 3.28
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(value)         => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
    
    // Ex 3.29
    def fold[A, B](tree: Tree[A])(leafTransformer: A => B)(branchTransformer: (B, B) => B): B = tree match {
      case Leaf(value) => leafTransformer(value)
      case Branch(left, right) => branchTransformer(fold(left)(leafTransformer)(branchTransformer),
        fold(right)(leafTransformer)(branchTransformer))
    }

    def maxIntViaFold(tree: Tree[Int]): Int = fold(tree)(value => value)(_ max _)
    
    def mapViaFold[A, B](tree: Tree[A])(f: A=> B): Tree[B] = fold(tree)(a=> Leaf(f(a)): Tree[B])(Branch(_,_))
  }

  val tree = Branch(Branch(Leaf(10), Leaf(20)), Branch(Leaf(30), Branch(Leaf(40), Leaf(50))))
  println("size: " + Tree.size(tree))
  println("max: " + Tree.maxInt(tree))
  println("depth: " + Tree.depth(tree))
  println("map: " + Tree.map(tree)(_+1))
  println("maxViaFold: " + Tree.maxIntViaFold(tree))
  println("mapViaFold: " + Tree.mapViaFold(tree)(_+1))
}