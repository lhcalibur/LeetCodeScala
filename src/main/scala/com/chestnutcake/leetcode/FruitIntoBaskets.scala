package com.chestnutcake.leetcode

// 904
object FruitIntoBaskets {

  //  res - the best found solution
  //  p1 - the previous element
  //  l1 - length of the longest suffix consisting only of p1
  //  p2 - an element before the previous, the longest suffix consisting of p1 and p2 is what we are searching for
  //  l2 - length of the longest suffix consisting of p1 and p2
  //  t - the current type of fruit
  def totalFruitV2(tree: Array[Int]): Int = tree
    .foldLeft((0, -1, 0, -1, 0)) { case ((res, p1, l1, p2, l2), t) => t match {
      case `p1` => (res max (l2 + 1), p1, l1 + 1, p2, l2 + 1)
      case `p2` => (res max (l2 + 1), p2, 1, p1, l2 + 1)
      case _ => (res max (l1 + 1), t, 1, p1, l1 + 1)
    }
    }._1

  def totalFruit(tree: Array[Int]): Int = {
    totalFruit(tree.toList)
    //    totalFruitV2(tree)
    //    totalFruitScanThroughBlocks(tree.toList)
  }

  @scala.annotation.tailrec
  def takeRightWhile(list: List[Int], res: List[Int] = List.empty)(f: Int => Boolean): List[Int] = {
    list match {
      case init :+ last => if (f(last)) takeRightWhile(init, last :: res)(f)
      else res
      case Nil => res
    }
  }

  def totalFruit(tree: List[Int]): Int = {
    val maxK = 2
    val tmp = tree.foldLeft((List.empty[Int], 0, 0))((b, a) => {
      b match {
        case (Nil, _, _) => (a :: Nil, 1, 1)
        case (l, k, c) if l contains a => (l :+ a, k, c)
        case (l, k, c) if k == maxK =>
          val newL = takeRightWhile(l, List())(_ == l.last) :+ a
          (newL, newL.distinct.length, c max l.length)
        case (l, k, c) => (l :+ a, k + 1, c)
      }
    })
    tmp._1.length max tmp._3
  }

  def totalFruitScanThroughBlocks(tree: List[Int]): Int = {

    @scala.annotation.tailrec
    def calMax(list: List[(Int, Int)], set: Set[Int], acc: Int, index: Int = 0): (Int, Int) = {
      list match {
        case x :: xs =>
          val newSet = set + x._1
          if (newSet.size > 2) (acc, index)
          else calMax(xs, newSet, acc + x._2, index + 1)
        case Nil => (acc, index)
      }
    }

    @scala.annotation.tailrec
    def iterCalMax(list: List[(Int, Int)], res: List[Int]): Int = {
      list match {
        case x :: xs =>
          val (acc, index) = calMax(x :: xs, Set(), 0)
          val newRes = res :+ acc
          if (index - 1 == 0)
            newRes.max
          else
            iterCalMax(list.drop(index - 1), newRes)
        case Nil => res.max
      }
    }

    val block = tree.foldLeft(List.empty[(Int, Int)])((b, a) => {
      b match {
        case Nil => (a, 1) :: Nil
        case init :+ last =>
          last match {
            case (blockK, blockV) =>
              if (blockK == a) init :+ (blockK, blockV + 1)
              else init :+ last :+ (a, 1)
          }
      }
    })

    iterCalMax(block, List())
  }
}
