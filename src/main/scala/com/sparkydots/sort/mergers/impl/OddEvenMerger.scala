package com.sparkydots.sort.mergers.impl

import com.sparkydots.sort.mergers.Merger

/**
 * Not tailrec
 */
trait OddEvenMerger extends Merger {
  def merge(xs: Seq[Int], ys: Seq[Int]): Seq[Int] = {
    xs.size match {
      case 0 => xs ++ ys
      case 1 =>
        (xs, ys) match {
          case (x +: Nil, y +: Nil) =>
            if (x < y) xs ++ ys else ys ++ xs
        }
      case _ =>

        val xsOdd = xs.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
        val xsEven = xs.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)
        val ysOdd = ys.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
        val ysEven = ys.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)

        val mergedOdd = merge(xsOdd, ysOdd)
        val mergedEven = merge(xsEven, ysEven)

        (mergedOdd, mergedEven) match {
          case (a1 +: as, bs :+ bn) =>
            a1 +: as.zip(bs).flatMap { case (a, b) => if (a < b) Seq(a, b) else Seq(b, a)} :+ bn
        }
    }
  }

}
