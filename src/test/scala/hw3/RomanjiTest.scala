package hw3

import hw3.Main.romanji
import org.scalatest.{FunSuite, Matchers}

class RomanjiTest extends FunSuite with Matchers {
  test("Toilet")     { romanji("トイレ") shouldBe "toire"  }
  test("Television") { romanji("テレビ") shouldBe "terebi" }
  test("Drama")      { romanji("ドラマ") shouldBe "dorama" }
  test("Ice-cream")  { romanji("アイスクリーム") shouldBe "aisukurīmu" }
  test("Knock")      { romanji("ノック") shouldBe "nokku"}

  test("empty string should return empty string") {
    romanji("") shouldBe ""
  }

  test("null should throw exception") {
    assertThrows[IllegalArgumentException](romanji(null))
  }

  test("hellomisa") {
    romanji("ヘローミーザー") shouldBe "herōmīzā"
  }

  test("bioop") {
    romanji("ビウープ") shouldBe "biūpu"
  }

  test("examples") {
    romanji("サカ") shouldBe "saka"
    romanji("サッカ") shouldBe "sakka"
    romanji("ナノ") shouldBe "nano"
    romanji("ナンノ") shouldBe "nanno"
    romanji("メル") shouldBe "meru"
    romanji("メール") shouldBe "mēru"
  }
}