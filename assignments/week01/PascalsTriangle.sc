/** Implementation of a function that computes the elements of Pascal's
  * triangle by means of a recursive process.
  *
  * @author Sebastian.J.Herzig
  */
object PascalsTriangle {

  /** Compute value in Pascal's triangle for a given column and row of the
    * triangle.
    *
    * Note that both rows and columns start from 0. If a value outside the
    * triangle is given, it is assumed 0.
    *
    * @param c The column index (starting from 0)
    * @param r The row index (starting from 0)
    * @return The value in Pascal's triangle if the row and column index
    *         are both valid, or an exception if not
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 && r == 0) { 1 }
    else if (!isValidEntry(c, r)) { 0 }
    else { pascal(c - 1, r - 1) + pascal(c, r - 1) }


  /** Checks whether a given row and column index maps to a valid entry
    * (some value > 0) in Pascal's triangle.
    *
    * @param c The column index (starting from 0)
    * @param r The row index (starting from 0)
    * @return TRUE if the indices are valid, FALSE otherwise
    */
  def isValidEntry(c: Int, r: Int): Boolean =
    if (c > r || c < 0 || r < 0) false else true
    // Or simply: !(c > r || c < 0 || r < 0)

  /** Test cases */
  pascal(0, 2)      // Should be 1
  pascal(1, 2)      // Should be 2
  pascal(1, 3)      // Should be 3




  /** For fun: a function that renders part of a Pascal's triangle up
    * to a specified depth.
    *
    * @param s The line to start from
    * @param d The depth of the Pascal's triangle (number of lines to
    *          render)
    * @return A string representation of the Pascal's triangle
    */
  def renderPascalsTriangle(s: Int, d: Int): String = {
    if (s > d) " "
    else { renderSinglePascalsTriangleLine(0, s) + "\n" +
    renderPascalsTriangle(s + 1, d) }
  }

  /** For fun: render a single line of a Pascal's triangle by specifying
    * the row to render (s) and the position to start from on the row (z).
    *
    * @param z Position to start in given line
    * @param s Row number to generate string representation of
    * @return String representation of Pascal's triangle line
    */
  def renderSinglePascalsTriangleLine(z: Int, s: Int): String =
    if (z > s) " "
    else pascal(z, s).toString() + " " + renderSinglePascalsTriangleLine(z + 1, s)

  /** Test cases */
  renderSinglePascalsTriangleLine(0, 4)     // Should be 1 4 6 4 1
  renderPascalsTriangle(0, 4)

}
