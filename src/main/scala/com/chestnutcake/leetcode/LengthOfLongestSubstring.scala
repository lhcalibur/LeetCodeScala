package com.chestnutcake.leetcode

object LengthOfLongestSubstring {
  def lengthOfLongestSubstring(s: String): Int = {
    @scala.annotation.tailrec
    def findLongest(s: String, i: Int = 0, j: Int = 0, map: Map[Char, Int] = Map.empty, res: Int = 0): Int = {
      if (j < s.length) {
        if (map.contains(s(j))) {
          val newI = i max map(s(j))
          findLongest(s, newI, j + 1, map.updated(s(j), j + 1), res max (j - newI + 1))
        }
        else
          findLongest(s, i, j + 1, map.updated(s(j), j + 1), res max (j - i + 1))
      } else
        res
    }

    findLongest(s, 0, 0, Map.empty, 0)
  }

  def lengthOfLongestSubstringV2(s: String): Int = {
    @scala.annotation.tailrec
    def recursive(curr: Int, maxLength: Int, map: Map[Char, Int]): Int =
      if (curr == s.length) maxLength
      else if (map contains s(curr)) {
        recursive(curr + 1,
          maxLength,
          (map -- map.filter(_._2 <= map(s(curr))).keys) ++ Map(s(curr) -> curr));
      } else {
        recursive(curr + 1,
          maxLength max (map.size + 1),
          map ++ Map(s(curr) -> curr))
      }

    recursive(0, 0, Map.empty)
  }

}
