package com.chestnutcake.leetcode

object CharacterReplacement {
  /*
  * In case anyone is confused by this solution, here's another way of explaining it:
  * end-start+1 = size of the current window
  * maxCount = largest count of a single, unique character in the current window
  * The main equation is: end-start+1-maxCount
  * When end-start+1-maxCount == 0, then then the window is filled with only one character
  * When end-start+1-maxCount > 0, then we have characters in the window that are NOT the character that occurs the most. end-start+1-maxCount is equal to exactly the # of characters that are NOT the character that occurs the most in that window. Example: For a window "xxxyz", end-start+1-maxCount would equal 2. (maxCount is 3 and there are 2 characters here, "y" and "z" that are not "x" in the window.)
  * We are allowed to have at most k replacements in the window, so when end-start+1-maxCount > k, then there are more characters in the window than we can replace, and we need to shrink the window.
  * If we have window with "xxxy" and k = 1, that's fine because end-start+1-maxCount = 1, which is not > k. maxLength gets updated to 4.
  * But if we then find a "z" after, like "xxxyz", then we need to shrink the window because now end-start+1-maxCount = 2, and 2 > 1. The window becomes "xxyz".
  * maxCount may be invalid at some points, but this doesn't matter, because it was valid earlier in the string, and all that matters is finding the max window that occurred anywhere in the string. Additionally, it will expand if and only if enough repeating characters appear in the window to make it expand. So whenever it expands, it's a valid expansion.
  *
  * This solution is great, best so far. However, it requires a bit more explanation.
  * Since we are only interested in the longest valid substring, our sliding windows need not shrink, even if a window may cover an invalid substring. We either grow the window by appending one char on the right, or shift the whole window to the right by one.
  * And we only grow the window when the count of the new char exceeds the historical max count (from a previous window that covers a valid substring).
  * That is, we do not need the accurate max count of the current window; we only care if the max count exceeds the historical max count; and that can only happen because of the new char.
  * Here's my implementation that's a bit shorter
  * */
  def characterReplacement(s: String, k: Int): Int = {
    s.length - s.zipWithIndex.foldLeft((Map.empty[Char, Int].withDefaultValue(0), 0, 0)) {
      case ((map, start, maxLength), (a, end)) =>
        val newMap = map ++ Map(a -> (map(a) + 1))
        val newMaxLength = maxLength max (newMap(a))

//        println(s)
//        println(s"${" " * (start - 0)}${"_" * (end - start + 1)}${" " * (s.length - end - 1)} len: ${end - start + 1} s: ${start} e: ${end} max: ${newMaxLength} cK: ${end - start + 1 - newMaxLength} k: ${k}")
        if (end - start + 1 - newMaxLength > k)
          (newMap ++ Map(s(start) -> (newMap(s(start)) - 1)), start + 1, newMaxLength)
        else
          (newMap, start, newMaxLength)
    }._2
  }
}
