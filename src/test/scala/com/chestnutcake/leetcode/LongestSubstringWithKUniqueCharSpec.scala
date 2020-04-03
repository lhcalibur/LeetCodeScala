package com.chestnutcake.leetcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import LongestSubstringWithKUniqueChar.longestSubstringWithKUniqueChar

class LongestSubstringWithKUniqueCharSpec extends AnyFlatSpec with Matchers {
  "\"aabbcc\", k = 1" should "return either oneof {\"aa\", \"bb\", \"cc\"}" in {
    val s = "aabbcc"
    val k = 1
    longestSubstringWithKUniqueChar(s, k) should (equal("aa") or equal("bb") or equal("cc"))
  }

  "\"aabbcc\", k = 2" should "return either oneof {\"aabb\", \"bbcc\"}" in {
    val s = "aabbcc"
    val k = 2
    longestSubstringWithKUniqueChar(s, k) should (equal("aabb") or equal("bbcc"))
  }

  "\"aabbcc\", k = 3" should "return \"aabbcc\"" in {
    val s = "aabbcc"
    val k = 3
    longestSubstringWithKUniqueChar(s, k) shouldEqual "aabbcc"
  }

  "\"aabacbebebe\", k = 3" should "return \"cbebebe\"" in {
    val s = "aabacbebebe"
    val k = 3
    longestSubstringWithKUniqueChar(s, k) shouldEqual "cbebebe"
  }
}
