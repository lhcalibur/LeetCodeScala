package com.chestnutcake.leetcode

object MaximumSubarray {

  import math.round

  def maximumSubarray(nums: Array[Int]): Int = {
    maximumSubarray(nums.toList)
  }

  def maximumSubarray(nums: List[Int]): Int = {
    nums match {
      case x :: Nil => x
      case _ =>
        val (a, b) = nums.splitAt(round(nums.length / 2))
        maximumSubarray(a) max maximumSubarray(b) max maximumCrossArray(a, b)
    }
  }

  private def maximumCrossArray(l: List[Int], r: List[Int]): Int = {
    List(l.reverse, r).map(
      a =>
        a.foldLeft((0, Int.MinValue))((b, a) => {
          val acc = b._1 + a
          val sum = b._2 max acc
          (acc, sum)
        })._2
    ).sum
  }
}