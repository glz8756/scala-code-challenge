package com.intenthq.challenge

object SNiceStrings {

  // From http://adventofcode.com/day/5
  //  --- Day 5: Doesn't He Have Intern-Elves For This? ---
  //
  //  Santa needs help figuring out which strings in his text file are naughty or nice.
  //
  //    A nice string is one with all of the following properties:
  //
  //    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
  //  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
  //    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
  //    For example:
  //
  //    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
  //  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
  //    jchzalrnumimnmhp is naughty because it has no double letter.
  //    haegwjzuvuyypxyu is naughty because it contains the string xy.
  //    dvszwmarrgswjxmb is naughty because it contains only one vowel.
  //    How many strings are nice?

  val vowels = "aeiou"
  val badSubStr = List("ab", "cd", "pq", "xy")

  def hasThreeVowels(in: String): Boolean = in.map { x => if (vowels.contains(x)) 1 else 0 }.sum >= 3

  def noBadSubStr(in: String): Boolean = badSubStr.forall(x => !in.contains(x))

  def hasDoubleLetter(xs: String): Boolean = xs.sliding(2).count(p => p(0) == p(1)) >= 1

  def isNice(in: String): Boolean = hasDoubleLetter(in) && noBadSubStr(in) && hasThreeVowels(in)

  def nice(xs: List[String]): Int = {
    xs.map(x => isNice(x)).foldLeft(0) { (acc, ele) => if (ele) acc + 1 else acc }
  }

}
