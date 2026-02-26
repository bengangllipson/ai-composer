package com.bengangllipson.aicomposer

import scala.util.Try

private trait Counterpoint {
  def generate(cantusFirmus: List[String] = List()): Try[List[String]]
  def formatOutput(line: List[String]): List[String]
}
