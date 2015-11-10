/** Implementation of algorithms that verify the balancing of parentheses in
  * a string. THe string is represented as a list of characters (List[Char]).
  *
  * @author Sebastian.J.Herzig
  */
object PascalsTriangle {

  /** Function that verifies balancing of parentheses in a string. The input
    * string is represented by a list of characters.
    *
    * @param chars The input string to check
    * @return TRUE if the parentheses are balanced, FALSE otherwise
    */
  def balance(chars: List[Char]): Boolean = {
    // Returns the number of open / closed parentheses found by adding 1 to
    // the result if an opening one was found, and subtracting one if a closing
    // parenthesis was found
    // For fun: define comparator for opening and closing brackets as anonymous
    // functions (can then be replaced for balancing any type of character)
    // (should ideally be defined further outside scope, could be generalized
    // further, but is just for practice)
    def balanceIter(openingFunc: Char => Boolean,
                    closingFunc: Char => Boolean,
                    currentIndex: Int): Int = {
      // Not optimal:
      //  - Scans entire string; should terminate if number of open parentheses
      //    ever drops below 0
      if (chars(currentIndex).equals("("))
        balanceIter(openingFunc, closingFunc, currentIndex + 1) + 1
      else if (chars(currentIndex).equals(")"))
        balanceIter(openingFunc, closingFunc, currentIndex + 1) - 1
      else 0
    }
    // Only balanced if result is 0
    balanceIter(isOpeningParenthesis,
                isClosingParenthesis,
                0) == 0
  }

  /** For fun: comparator functions */
  def isOpeningParenthesis(char: Char): Boolean =
    if (char.equals("(")) true else false

  def isClosingParenthesis(char: Char): Boolean =
    if (char.equals(")")) true else false

  /** Test cases */
  balance("(if (zero? x) max (/ 1 x))".toList) == true
  balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList) == true

}