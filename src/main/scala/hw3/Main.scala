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

    //only letters
    val letters = corpus.filter(x => x.isLetter).map(x => x.toLower).toList

    if(letters.isEmpty)
      return ""

    //sort alphabetically
    val sorted = letters.sortWith((x,y) => x.toInt < y.toInt)

    //returns letters with frequencies
    @scala.annotation.tailrec
    def loop(acc:List[(Char, Int)], rest: List[Char], count: Int, current: Char): List[(Char, Int)] = {
      if(rest.isEmpty) (current, count) :: acc
      else if (rest.head == current) loop(acc, rest.tail, count+1, current)
      else loop((current, count) :: acc, rest.tail, 1, rest.head)
    }

    val counts = loop(List[(Char, Int)](), sorted.tail, 1, sorted.head).reverse

    //sorted by frequencies
    val sortedByCounts = counts.sortWith((x,y) => x._2 > y._2)

    sortedByCounts.foldLeft("")((acc,x) => acc + x._1)
  }

  def romanji(katakana: String): String = {
    if(katakana == null)
      throw new IllegalArgumentException("cannot convert non-existing string")

    def add(adding: List[Char]): String = adding.foldLeft("")((acc,x) => acc + x)

    def loop(acc: String, rest: List[Char]): String = {
      if(rest.isEmpty) return acc
      rest.head match {
        case 'ー' if Katakana.longVowels.get(acc.last).isDefined =>
          val last = acc.last
          loop( acc.take(acc.length-1) + Katakana.longVowels(last), rest.tail)
        case 'ン' =>
          val restString = loop("", rest.tail)
          if(restString.startsWith("na") || restString.startsWith("ni") || restString.startsWith("nu") || restString.startsWith("ne") || restString.startsWith("no")) {
            acc + "n" + restString
          } else {
            acc + restString
          }
        case symbol if(List('ャ', 'ュ', 'ョ').contains(symbol) && acc.last == 'i' && Katakana.vowels.get(acc.charAt(acc.length-2)).isEmpty) =>
          loop(acc.take(acc.length-1) + add(Katakana.symbols(symbol)), rest.tail)
        case 'ッ' =>
          val restString = loop("", rest.tail)
          if(Katakana.vowels.get(restString.head).isEmpty)
            acc + restString.head + restString
          else
            acc + restString
        case symbol if Katakana.symbols.get(symbol).isDefined =>
          loop(acc + add(Katakana.symbols(symbol)), rest.tail)
        case _ => throw new IllegalArgumentException(s"cannot convert ${katakana} to Romanji")
      }
    }

    loop("", katakana.toList)
  }

  def gray(bits: Int): List[String] = {
    @scala.annotation.tailrec
    def loop(acc: List[String], n: Int): List[String] = {
      if(n == 0) acc
      else {
        val first: List[String] = acc.map(x => "0" + x)
        val second: List[String] = acc.reverse.map(x => "1" + x)
        loop( first concat second, n-1)
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

  val vowels = Map(
    'ア' -> List('a'), 'イ' -> List('i'),  'ウ' -> List('u'), 'エ' -> List('e'), 'オ' -> List('o')
  )
}