object sandbox {
  type Set = Int => Boolean

  def singletonSet(elem: Int): Set =
    Set(elem)
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = x => s(x) || t(x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = filter(s, t)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = filter(s, x => !t(x))

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)


  def s: Set = singletonSet(1)
  contains(s, 1)

  def t: Set = singletonSet(10)

  contains(union(s, t), 1)

  diff(s, t)
}