// Jeremy Udis
// 10-18-2017
package test.poker

object Poker extends App {

  /*
   * Given a set of 5 playing card identifiers such as 2H, 7C, QS, 10D, 2D;
   * determine if this hand is better than some other hand, according to the rules of poker.
   *
   * Hands will be a string with 5 cards comma separated,
   * each card will have 1-2 digits or JQKA and a suit indicator C,D,S,H (i.e. 10C, KH)
   *
   * Possible Hand Types Below:
   *   Straight flush
   *   Four of a kind
   *   Full house
   *   Flush
   *   Straight
   *   Three of a kind
   *   Two pair
   *   One pair
   *
   * The goal of this is to compare between the hand types.
   * Comparing 2 of the same type (i.e. 2 straights) to determine a winner is outside the scope
   * and will not be tested.
   *
   * Implement hand1WinsOverHand2 method and return whether or not the first hand wins over the second hand.
   */
  // helper to check if hand is a straight/straight flush
  def isConsecutive(seq: Array[Int]): Boolean = {
    for(i <- 0 to seq.length - 5) {
      if (seq(i) == 1) {
        for (j <- i+1 to i + 4) {
          if (seq(j) != 1)
            return false
        }
        return true
      }
    }
    return false
  }
  def calculateHandType(hand: String): Int = {
    /* Check hand types on a and Assign a Numerical Rank */

    // filter string and split into card rank / suit
    val newHand = hand.filterNot(_ == ',')
    val filterList = List[Char]('2','3','4','5','6','7','8','9','1','J','Q','K','A') //Note 1 in place of 0 as they occur same # of times 
    // split string into 2 new strings that contain only suit or only card rank
    val cardRank = newHand.filter(filterList.toSet)
    val suit = newHand.filterNot(filterList.toSet)
    val newSuit = suit.filterNot(_ == '0')
    
    var cardCount = new Array[Int](13)
    // fill in count values in order
    for(i <- 0 to filterList.length - 1) {
      cardCount(i) = cardRank.count(_ == filterList(i))  
    }
    // count of each suit
    val suitCount:Map[Char, Int] = newSuit.groupBy(identity).mapValues(_.size)
    // greedy strategy, match on highest card count, suit count
    val maxSuitCount = suitCount.valuesIterator.max
    val maxCardFreq = cardCount.max
    val rank = maxCardFreq match {
      case 4 | 5 => 7 // four of a kind
      case 3 => if (cardCount.contains(2)) 6 else 3 // if another card has count of 2, it's a full house, otherwise it's 3 of a kind  
      case 2 =>  if (cardCount.count(_ == 2) == 2) 2 else 1 //if two occurences of count 2 exist, two pairs, else only 1
      case 1 => maxSuitCount match {
        case 5 => if (isConsecutive(cardCount)) 8 else 5 // straight flush or flush
        case _ => if (isConsecutive(cardCount)) 4 else 0 // straight or nothing
      }
      case _ => 0      
    }
    return rank
  }
  def hand1WinsOverHand2(hand1Str: String, hand2Str: String): Boolean = {
    // cache rank values for each hand and compare them
    val hand1Rank = calculateHandType(hand1Str)
    val hand2Rank = calculateHandType(hand2Str)
    if (hand1Rank > hand2Rank) true else false
  }
  implicit class CompareTwoPokerHands(hand1: String) {
    def winsOver(hand2: String): Unit = {
      val result = if (hand1WinsOverHand2(hand1, hand2)) "Correct" else "Incorrect"
      println(s"$result, hand [$hand1] wins over [$hand2]")
    }
  }
  println("Poker Hand comparison")
  "8C,9C,10C,JC,QC" winsOver "6S,7H,8D,9H,10D" // straight flush
  "4H,4D,4C,4S,JS" winsOver "6C,6S,KH,AS,AD" // four of a kind
  "5C,3C,10C,KC,7C" winsOver "6C,6D,6H,9C,KD" // flush
  "4H,4D,4C,KC,KD" winsOver "9D,6S,KH,AS,AD" // full house
  "2C,3C,4S,5S,6S" winsOver "6C,6D,6H,9C,KD" // straight
  "7C,7D,7S,3H,4D" winsOver "9S,6S,10D,AS,AD" // three of a kind
  "8C,8H,10S,KH,KS" winsOver "2S,2D,JH,7S,AC" // two pair
  "AC,AH,3C,QH,10C" winsOver "3S,2D,KH,JS,AD" // one pair
  System.exit(0) //invokes an exception unless you fork JVM when running console app
}