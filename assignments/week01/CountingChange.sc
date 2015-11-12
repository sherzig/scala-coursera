/** Algorithms that calculate the various ways in which change can
  * be given out given a list of coin denominations.
  *
  * @author Sebastian.J.Herzig
  */
object CountingChange {

  /** Recursive function that counts how many different ways you can
    * make change for an amount, given a list of coin denominations.
    *
    * @param money The money to convert into change as an integer
    * @param coins The coin denominations as positive integers. It
    *              is not expected that the list is ordered
    * @return The number of different ways that one can make change
    *         for a particular amount
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins: List[Int] = coins.sortWith(_ > _)    // Sorted from greatest to smallest
    var validCombinations = 0

    /** Computes all possible combinations of coins as possible ways of
      * giving out change for a specified amount of money while avoiding
      * mathematical duplicates.
      *
      * The algorithm is as follows:
      * 1) Start with the first coin (first in a list from highest to
      *    lowest denomination)
      * 2) If the result of subtracting it is >= 0, do so, and go back to 1)
      * 3) If the result is smaller than one, do not subtract the coin value
      *    from the amount of money left, but call the function recursively
      *    with a lower denomination
      * 4) For every recursive call, repeat the procedure for every LOWER
      *    denomination other than the current one used (this avoids
      *    mathematically identical solutions and enumerates all possible
      *    combinations of coins)
      * 5) Whenever the amount of money reaches 0, mark the trial a success
      *    by adding '1' to a variable that is defined outside the scope of
      *    this recursive function
      *
      * Corner cases / cases being accounted / to test for:
      * 1) Amount is less than or equal to 0 => In this case, no combinations
      *    are possible and the function should return 0
      * 2) The highest denomination of an input coin is larger than the
      *    amount of money itself => use lower denominations
      * 3) The denominations for coins are all larger than the amount to be
      *    changed => no combinations possible
      * 4) Cannot pay out the exact amount using the given coin denominations
      *    => no combinations possible
      *
      * @param moneyLeft The amount of money left in the current call
      * @param currentDenomination The index in the list of sorted coins
      *                            as a representation of the current
      *                            denomination used
      * @return The number of possible combinations
      */
    def countChangeIter(moneyLeft: Int, currentDenomination: Int): Int = {
      if (moneyLeft == 0)
        // Gave correct change - make a record of it (note that the case where
        // no money is given is catched outside this recursive inner function)
        validCombinations = validCombinations + 1
      else if (moneyLeft - sortedCoins(currentDenomination) >= 0)
        // Can subtract coin value - do so, and recursively attempt to
        // subtract a) the same and b) any lower denomination as well
        // (count as separate successful combinations)
        for (i <- currentDenomination to (sortedCoins.length - 1))
          countChangeIter(moneyLeft - sortedCoins(i), i)
      else if (moneyLeft - sortedCoins(currentDenomination) < 0
              && currentDenomination + 1 < sortedCoins.length)    // End of list
        // Subtracting the current denomination would lead to a negative result
        // so attempt to subtract something smaller (if possible)
        countChangeIter(moneyLeft, currentDenomination + 1)

      // Return valid combinations found
      validCombinations
    }

    // Only give change if there is change to give
    if (money <= 0) 0 else countChangeIter(money, 0)
  }

  /** Test cases */
  countChange(4, List(1, 2)) == 3        // 2+2, 2+1+1, 1+1+1+1
  countChange(6, List(1, 2)) == 4        // 2+2+2, 2+2+1+1, 2+1+1+1+1, 1+1+1+1+1+1
  countChange(10,
    List(1, 2, 5, 8, 10, 17, 43)) == 13  // 10, 8+2, 8+1+1, 5+5, 5+2+2+1, 5+2+1+1+1, 5+1+1+1+1+1,
                                         // 2+2+2+2+2, 2+2+2+2+1+1, 2+2+2+1+1+1+1, 2+2+1+1+1+1+1+1
                                         // 2+1+1+1+1+1+1+1+1, 1+1+1+1+1+1+1+1+1+1

  countChange(100,
    List(1, 2, 5, 8, 10, 17, 43)) == 17522

  countChange(0, List(1, 2, 10, 50)) == 0 // Corner case
  countChange(10, List(100, 200)) == 0    // Corner case

}