package hw3

object Main {
  def standardDeviation(vector: List[Double]): Double = {
    if(vector == null || vector.isEmpty)
      throw new IllegalArgumentException("Cannot count deviation")

    import Math._
    val size: Double = vector.size.toDouble
    val mean: Double = vector.sum / size
    val variance = vector.map(x => pow(x-mean, 2)).sum / size
    sqrt(variance)
  }

  def letterFrequencyRanking(corpus: String): String = {
    if(corpus == null)
      throw new IllegalArgumentException("cannot do the ranking for non-existing string")
    if(corpus.isEmpty)
      return corpus

    def makeString(map: Map[Char, Int]): String = ???

    val symbols = corpus.toList
    def loop(list: List[Char], map: Map[Char, Int]): String = ???
//    {
//      if(list.isEmpty) makeString(map)
//      else if(list.head.isLetter) {
//        loop(list.tail, )
//      } else {
//        loop(list.tail, map)
//      }
//    }
    "not implemented"
  }

  def romanji(katakana: String): String = {
    if(katakana == null)
      throw new IllegalArgumentException("cannot convert non-existing string")

    def loop(acc: String, list: List[Char]): String = {
      if(list.isEmpty) return acc
      if(list.head == 'ー') {
          val toChange = acc.last
          val newVal = Katakana.longVowels(toChange)
          loop(acc.take(acc.length-1) + newVal, list.tail)
      }
      else if(list.head == 'ッ'){
        val rest = loop("", list.tail)
        acc + rest.head + rest
      }
      else {
        val toAdd = Katakana.symbols(list.head).foldLeft("")((acc, x) => acc + x)
        loop(acc + toAdd, list.tail)
      }
    }

    val symbols = katakana.toList
    if(!symbols.forall(x => {
      Katakana.symbols.get(x).isDefined || Katakana.longVowels.get(x).isDefined || x == 'ッ' || x == 'ー'
    }))
      throw new IllegalArgumentException("invalid string, cannot resolve all symbols")

    loop("", symbols)
  }

  def gray(bits: Int): List[String] = {
    @scala.annotation.tailrec
    def loop(acc: List[String], n: Int): List[String] = {
      if(n == 0) acc
      else {
        val reversed = acc.reverse
        val first: List[String] = acc.map(x => "0" + x)
        val second: List[String] = reversed.map(x => "1" + x)
        loop( first ::: second, n-1)
      }
    }

    if(bits < 1)
      throw new IllegalArgumentException(s"Cannot make gray code of $bits.")
    loop(List("0","1"), bits-1)
  }
}

object Katakana {
  val symbols = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o'),
    'ン' -> List('n'),
    'カ' -> List('k','a'), 'キ' -> List('k','i'), 'ク' -> List('k','u'), 'ケ' -> List('k','e'), 'コ' -> List('k','o'),
    'ガ' -> List('g','a'), 'ギ' -> List('g','i'), 'グ' -> List('g','u'), 'ゲ' -> List('g','e'), 'ゴ' -> List('g','o'),
    'サ' -> List('s','a'), 'シ' -> List('s','i'), 'ス' -> List('s','u'), 'セ' -> List('s','e'), 'ソ' -> List('s','o'),
    'ザ' -> List('z','a'), 'ジ' -> List('z','i'), 'ズ' -> List('z','u'), 'ゼ' -> List('z','e'), 'ゾ' -> List('z','o'),
    'タ' -> List('t','a'), 'チ' -> List('t','i'), 'ツ' -> List('t','u'), 'テ' -> List('t','e'), 'ト' -> List('t','o'),
    'ダ' -> List('d','a'), 'ヂ' -> List('d','i'), 'ヅ' -> List('d','u'), 'デ' -> List('d','e'), 'ド' -> List('d','o'),
    'ナ' -> List('n','a'), 'ニ' -> List('n','i'), 'ヌ' -> List('n','u'), 'ネ' -> List('n','e'), 'ノ' -> List('n','o'),
    'ハ' -> List('h','a'), 'ヒ' -> List('h','i'), 'フ' -> List('h','u'), 'ヘ' -> List('h','e'), 'ホ' -> List('h','o'),
    'バ' -> List('b','a'), 'ビ' -> List('b','i'), 'ブ' -> List('b','u'), 'ベ' -> List('b','e'), 'ボ' -> List('b','o'),
    'パ' -> List('p','a'), 'ピ' -> List('p','i'), 'プ' -> List('p','u'), 'ペ' -> List('p','e'), 'ポ' -> List('p','o'),
    'マ' -> List('m','a'), 'ミ' -> List('m','i'), 'ム' -> List('m','u'), 'メ' -> List('m','e'), 'モ' -> List('m','o'),
    'ヤ' -> List('y','a'),                        'ユ' -> List('y','u'),                        'ヨ' -> List('y','o'),
    'ラ' -> List('r','a'), 'リ' -> List('r','i'), 'ル' -> List('r','u'), 'レ' -> List('r','e'), 'ロ' -> List('r','o'),
    'ワ' -> List('w','a'), 'ヰ' -> List('w','i'),                        'ヱ' -> List('w','e'), 'ヲ' -> List('w','o'),
  )
  val longVowels = Map(
    'a' -> 'ā',
    'i' -> 'ī',
    'e' -> 'ē',
    'u' -> 'ū',
    'o' -> 'ō'
  )
}