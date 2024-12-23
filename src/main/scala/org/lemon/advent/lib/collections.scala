package org.lemon.advent.lib

extension [A](it: Iterator[A])
  /** Returns the nth element of the iterator.
    *
    * @param n the index of the element to return
    * @throws NoSuchElementException if there is no nth element
    * @return the nth element of the iterator
    */
  def nth(n: Int): A = it.drop(n).next

  /** Returns the nth element of the iterator, or None if there is no nth element.
    *
    * @param n the index of the element to return
    * @return the nth element of the iterator, or None if there is no nth element
    */
  def nthOption(n: Int): Option[A] = it.drop(n).nextOption
