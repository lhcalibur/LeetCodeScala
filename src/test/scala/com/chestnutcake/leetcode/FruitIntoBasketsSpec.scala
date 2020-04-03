package com.chestnutcake.leetcode

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import FruitIntoBaskets.totalFruit

class FruitIntoBasketsSpec extends AnyFlatSpec with Matchers {
  "Array(1,0,1,4,1,4,1,2,3)" should "return 5" in {
    val numbers = Array(1, 0, 1, 4, 1, 4, 1, 2, 3)
    totalFruit(numbers) shouldEqual 5
  }
}
