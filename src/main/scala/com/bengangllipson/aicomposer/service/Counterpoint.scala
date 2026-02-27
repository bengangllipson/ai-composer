package com.bengangllipson.aicomposer.service

import scala.util.Try

private[aicomposer] trait Counterpoint {
  def generate(cantusFirmus: List[String] = List()): Try[List[String]]

  def formatOutput(line: List[String]): List[String]
}
