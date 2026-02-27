package com.bengangllipson.aicomposer.api

import com.bengangllipson.aicomposer.service.{CantusFirmusService, FirstSpeciesService}

import scala.util.{Failure, Success}

class AiComposer {
  private lazy val cantusFirmusService = new CantusFirmusService()
  private lazy val firstSpeciesService = new FirstSpeciesService()

  def generateCantusFirmus(): List[String] = {
    cantusFirmusService.generate() match {
      case Success(value) => value
      case Failure(exception) => throw exception
    }
  }

  def generateFirstSpecies(cantusFirmus: List[String]): List[String] = {
    firstSpeciesService.generate(cantusFirmus) match {
      case Success(value) => value
      case Failure(exception) => throw exception
    }
  }
}
