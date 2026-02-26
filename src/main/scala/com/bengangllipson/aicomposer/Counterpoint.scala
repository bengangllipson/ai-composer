package com.bengangllipson.aicomposer

import scala.util.Try

trait Counterpoint {
  def generate(cantusFirmus: List[String] = List()): Try[List[String]]
  def formatOutput(line: List[String]): List[String]
}
