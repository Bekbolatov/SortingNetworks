package com.sparkydots.sort.mergers.impl

import com.sparkydots.sort.mergers.Merger

/**
 * Not tailrec
 */
trait BitonicMerger extends Merger {

  def merge(xs: Seq[Int], ys: Seq[Int]): Seq[Int] = {
    bitonicMerge(xs ++ ys.reverse)
  }

  private def bitonicMerge(xs: Seq[Int]): Seq[Int] = {

    xs.size match {
      case 0 => xs
      case 1 => xs
      case _ =>
        val (lefts, rights) = xs.splitAt(xs.size / 2)

        var lower = List[Int]()
        var upper = List[Int]()

        lefts.zip(rights).foreach { case (l, r) =>
          if (l < r) {
            lower = lower :+ l
            upper = upper :+ r
          } else {
            lower = lower :+ r
            upper = upper :+ l
          }
        }

        bitonicMerge(lower) ++ bitonicMerge(upper)
    }
  }

}
