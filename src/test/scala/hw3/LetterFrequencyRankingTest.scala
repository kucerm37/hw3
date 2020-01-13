package hw3

import hw3.Main.letterFrequencyRanking
import org.scalatest.{FunSuite, Matchers}

class LetterFrequencyRankingTest extends FunSuite with Matchers {
  test("Simple")          { letterFrequencyRanking("hello") shouldBe "leho" }
  test("Capital letters") { letterFrequencyRanking("AaaAaaAaa") shouldBe "a" }
  test("Punctuation")     { letterFrequencyRanking("Sic!") shouldBe "cis" }

  test("empty text returns empty string") {
    letterFrequencyRanking("") shouldBe ""
  }

  test("pass null throws exception") {
    assertThrows[Exception](letterFrequencyRanking(null))
  }

  test("pass text with other characters") {
    letterFrequencyRanking("123") shouldBe ""
    letterFrequencyRanking("p1i2z3z a") shouldBe "zaip"
  }
}