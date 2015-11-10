/** Implementation of algorithms that verify the balancing of parentheses in
  * a string. The string is represented as a list of characters (List[Char]).
  *
  * @author Sebastian.J.Herzig
  */
object ParenthesesBalancing {

  /** Function that verifies balancing of parentheses in a string. The input
    * string is represented by a list of characters.
    *
    * @param chars The input string to check
    * @return TRUE if the parentheses are balanced, FALSE otherwise
    */
  def balance(chars: List[Char]): Boolean = {

    /** Returns the number of open / closed parentheses found by adding 1 to
      * the result if an opening one was found, and subtracting one if a closing
      * parenthesis was found.
      *
      * For fun: define comparator for opening and closing brackets as anonymous
      * functions (can then be replaced for balancing any type of character)
      * (should ideally be defined further outside scope, could be generalized
      * further, but is just for practice).
      *
      * @param openingFunc Function that checks whether or not the given character
      *                    represents an opening parenthesis / bracket
      * @param closingFunc Function that checks whether or not the given character
      *                    represents an opening parenthesis / bracket
      * @param currentIndex The current index in the list of characters representing
      *                     the input string
      * @param numOpen The number of open parentheses / brackets
      * @return The number of open / closed parentheses / brackets relative to
      *         a starting value of 0. -1 is returned if, at any point, the number
      *         of open / closed brackets relative to 0 drops to a negative value.
      *         This is done so that a) the algorithm terminates more quickly on
      *         average and b) to catch situations in which an opening bracket does
      *         not precede a closing bracket (e.g., "())(")
      */
    def balanceIter(openingFunc: Char => Boolean,
                    closingFunc: Char => Boolean,
                    currentIndex: Int,
                    numOpen: Int): Int = {
      if (currentIndex >= chars.length) numOpen     // End of string
      else if (numOpen < 0) -1      // Opening parenthesis must precede closing
      else if (openingFunc(chars(currentIndex)))    // Is opening parenthesis?
        balanceIter(openingFunc, closingFunc, currentIndex + 1, numOpen + 1)
      else if (closingFunc(chars(currentIndex)))    // Is closing parenthesis?
        balanceIter(openingFunc, closingFunc, currentIndex + 1, numOpen - 1)
      else balanceIter(openingFunc, closingFunc, currentIndex + 1, numOpen)
    }

    /* Only balanced if result is 0 */
    balanceIter(isOpeningParenthesis,
                isClosingParenthesis,
                0,
                0) == 0
  }

  /** For fun: comparator functions */
  def isOpeningParenthesis(char: Char): Boolean =
    if (char.equals('(')) true else false

  def isClosingParenthesis(char: Char): Boolean =
    if (char.equals(')')) true else false

  /** Test cases */
  balance("(if (zero? x) max (/ 1 x))".toList) == true
  balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList) == true
  balance(") not balanced".toList) == false
  balance(") also not balanced (".toList) == false
  balance(":-)".toList) == false
  balance("())(".toList) == false

}