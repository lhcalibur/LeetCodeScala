package com.chestnutcake.leetcode

// nonfree
object LongestSubstringWithKUniqueChar {
  def longestSubstringWithKUniqueChar(s: String, k: Int): String = {
    longestSubstringWithKUniqueChar(s.toList, k).mkString
  }

  def longestSubstringWithKUniqueChar(s: List[Char], maxK: Int): List[Char] = {
    s.foldLeft((List.empty[Char], 0, List.empty[Char]))((b, a) => {
      val tmp = b match {
        case (Nil, _, _) => (a :: Nil, 1)
        case (l, k, _) if l.contains(a) => (l :+ a, k)
        case (l, k, _) if k == maxK =>
          (l.drop(l.lastIndexOf(l.head) + 1) :+ a, k)
        case (l, k, _) => (l :+ a, k + 1)
      }
      tmp match {
        case (l, k) =>
          if (l.length > b._3.length) (l, k, l)
          else (l, k, b._3)
      }
    })._3
  }
}
