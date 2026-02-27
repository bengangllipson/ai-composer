package com.bengangllipson.aicomposer.service

import scala.util.Random

private[aicomposer] class RandomService {
  def between(minInclusive: Int, maxExclusive: Int): Int = Random.between(minInclusive, maxExclusive)

  def nextDouble(): Double = Random.nextDouble()

  def nextInt(n: Int): Int = Random.nextInt(n)
}
