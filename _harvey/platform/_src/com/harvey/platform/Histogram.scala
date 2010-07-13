package com.harvey.platform

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

trait Histogram {
  def increment(bucket: Int): Unit
  def bucket(bucket: Int): Int
  def buckets(): List[Int]
}

case class ListHistogram(size: Int) extends Histogram {
  val histogram = new ListBuffer[Int]
  
  // initialize the buckets in the histogram to 0
  for (i <- 0 to size-1) {
    histogram += 0
  }
  
  def increment(bucket: Int) {
    histogram(bucket) = histogram(bucket) + 1
  }
  def bucket(bucket: Int) = histogram(bucket)
  def buckets() = histogram.toList
  override def toString() = histogram.mkString(",")
}

object HistogramAccessor {
  val histograms = Map[Int,Histogram]()
  def apply(key: Int) = histograms(key)
  def install(key: Int, histogram: Histogram) {
    histograms += (key -> histogram)
  }
  def uninstall(key: Int) {
    histograms -= key
  }
  def uninstall() {
    histograms.clear
  }
}
