package com.sparkydots.sort.combinedSorters

object BitonicSorter {

  /**
   * Assume xs.size is always a power of 2
   * @param xs
   * @return
   */
  def sort(xs: Seq[Int]): Seq[Int] = {
    xs.size match {
      case 1 => xs
      case _ =>

        // divide input data into pieces and compute results
        val (lefts, rights) = xs.splitAt(xs.size / 2)
        val sortedLefts = sort(lefts) //work
        val sortedRights = sort(rights) //work

        bitonicSort(sortedLefts ++ sortedRights.reverse)
    }
  }

  def bitonicSort(xs: Seq[Int]): Seq[Int] = {

    xs.size match {
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

        bitonicSort(lower) ++ bitonicSort(upper)
    }
  }

}
