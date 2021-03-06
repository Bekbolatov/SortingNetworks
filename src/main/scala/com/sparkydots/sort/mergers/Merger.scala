package com.sparkydots.sort.mergers

trait Merger {
  def merge(xs: Seq[Int], ys: Seq[Int]): Seq[Int]
}

trait MergeSorting {
  self: Merger =>

  def sort(xs: Seq[Int]): Seq[Int] = {
    if (xs.size < 2) {
      xs
    } else {
      val (lefts, rights) = xs.splitAt(xs.size / 2)
      merge(sort(lefts), sort(rights))
    }
  }

}