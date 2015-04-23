package com.sparkydots.sort.mergers.impl

import com.sparkydots.sort.mergers.Merger

import scala.annotation.tailrec

trait LinearMerger extends Merger {
  def merge(xs: Seq[Int], ys: Seq[Int]): Seq[Int] = {
    linearMerge(xs, ys, Nil)
  }

  @tailrec
  private final def linearMerge(xs: Seq[Int], ys: Seq[Int], merged: List[Int]): Seq[Int] = {
    (xs, ys) match {
      case (Nil, Nil) => merged
      case (_, Nil) => merged ++ xs
      case (Nil, _) => merged ++ ys
      case (x +: xx, y +: yy) =>
        val (newXs, newYs, newMerged) = if (x > y) {
          (xs, yy, merged :+ y)
        } else {
          (xx, ys, merged :+ x)
        }
        linearMerge(newXs, newYs, newMerged)
    }
  }
}
