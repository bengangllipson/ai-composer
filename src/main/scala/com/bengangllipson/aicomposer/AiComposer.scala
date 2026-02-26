package com.bengangllipson.aicomposer

import scala.util.Try

class AiComposer {
  def generateCantusFirmus(): Try[List[String]] = {
    new CantusFirmusService().generate()
  }
}
