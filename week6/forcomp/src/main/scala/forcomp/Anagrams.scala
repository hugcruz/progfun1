package forcomp


object Anagrams {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.toList.groupBy(c => c).map(entry => (entry._1,entry._2.size)).toList.sorted

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.foldLeft("")((result: Word, current: Word) => result + current))

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(w => wordOccurrences(w))

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.get(wordOccurrences(word)).get

  def combinations(occurrences: Occurrences): List[Occurrences] = cleanup(findCombinations(occurrences))

  def findCombinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List(('x', 0)))
    case head :: tail => 
      for {
        comb <- findCombinations(tail)
        cnt <- 0 to head._2
      } yield ((head._1, cnt) :: comb)
  }

  def cleanup(results: List[Occurrences]): List[Occurrences] = results.map((o:Occurrences) => o.filter(_._2 != 0))


  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */

  def subtract(x: Occurrences, y: Occurrences): Occurrences = x.foldLeft(List[(Char, Int)]()) ((b,a) => {
    def getSub(c: Char): Int = y.filter(_._1 == c).foldLeft(0)((cnt, a) => a._2)

    val sub = getSub(a._1)
    if(a._2 - sub == 0) b else b:::List((a._1, a._2-sub))
  })

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentence match {
    case Nil 	=> List(List())
    case _ 	=> {
      val o = sentenceOccurrences(sentence)
      val combinations = findCombinations(o)
      val sentences = dictionaryByOccurrences.get(combinations.head)
      
      combinations.flatMap((occur: Occurrences) => dictionaryByOccurrences.get(occur))
      //dictionaryByOccurrences: Map[Occurrences, List[Word]]
    }
  }
}
