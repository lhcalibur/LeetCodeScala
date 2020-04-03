package com.chestnutcake.leetcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import LengthOfLongestSubstring.lengthOfLongestSubstring

class LengthOfLongestSubstringSpec extends AnyFlatSpec with Matchers {
  "\"abcabcbb\"" should "return 3" in {
    val s = "abcabcbb"
    lengthOfLongestSubstring(s) shouldEqual 3
  }

  "\"bbbbb\"" should "return 1" in {
    val s = "bbbbb"
    lengthOfLongestSubstring(s) shouldEqual 1
  }

  "\"pwwkew\"" should "return 3" in {
    val s = "pwwkew"
    lengthOfLongestSubstring(s) shouldEqual 3
  }
}
