package com.bengangllipson.aicomposer.api

import com.bengangllipson.aicomposer.service.CantusFirmusService

import scala.util.{Failure, Success}

class AiComposer {
  private lazy val cantusFirmusService = new CantusFirmusService()

  def generateCantusFirmus(): List[String] = {
    cantusFirmusService.generate() match {
      case Success(value) => value
      case Failure(exception) => throw exception
    }
  }
}
