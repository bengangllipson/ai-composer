package com.bengangllipson.aicomposer.api

import com.bengangllipson.aicomposer.service.CantusFirmusService
import scala.util.Try

class AiComposer {
  private lazy val cantusFirmusService = new CantusFirmusService()
  def generateCantusFirmus(): Try[List[String]] = {
    cantusFirmusService.generate()
  }
}
