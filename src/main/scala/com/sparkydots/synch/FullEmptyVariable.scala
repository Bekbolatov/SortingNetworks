package com.sparkydots.synch

import java.util.concurrent.{LinkedBlockingQueue, CountDownLatch}

import scala.collection.mutable

/**
 * Full/Empty variables
 * Allows a queue or writes and reads
 * @tparam T
 */
class FE[T] {

  val queue = new LinkedBlockingQueue[T]()

  def :=(other: T): Unit = fill(other)

  def fill(other: T) = queue.put(other)

  def read() = queue.take()

}

