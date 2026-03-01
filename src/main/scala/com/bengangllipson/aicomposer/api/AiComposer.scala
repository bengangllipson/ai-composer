package com.bengangllipson.aicomposer.api

import com.bengangllipson.aicomposer.service.{CantusFirmusService, FirstSpeciesService}

import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success}

class AiComposer {
  private lazy val cantusFirmusService = new CantusFirmusService()
  private lazy val firstSpeciesService = new FirstSpeciesService()

  def generateCantusFirmus(): java.util.List[String] = {
    cantusFirmusService.generate() match {
      case Success(value) => value.asJava
      case Failure(exception) => throw exception
    }
  }

  def generateFirstSpecies(cantusFirmus: List[String]): java.util.List[String] = {
    firstSpeciesService.generate(cantusFirmus) match {
      case Success(value) => value.asJava
      case Failure(exception) => throw exception
    }
  }
}
