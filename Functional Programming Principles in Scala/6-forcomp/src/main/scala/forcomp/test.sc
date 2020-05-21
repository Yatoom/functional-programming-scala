import forcomp.Anagrams.Sentence

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
  match sentence {
    case List() => List[Sentence]()
    case sent: Sentence if sent.nonEmpty => (
      for (
        i <- sent.indices;
        anagrams <- sentenceAnagrams(sent.drop(i))
      ) yield sentence.take(i) :: anagrams
    ).toList
  }
}

List("I", "love", "you")