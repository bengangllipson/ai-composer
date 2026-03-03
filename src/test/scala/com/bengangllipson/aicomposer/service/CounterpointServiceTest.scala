package com.bengangllipson.aicomposer.service

import com.bengangllipson.aicomposer.service.CounterpointService.GET_ALL_NOTES_BETWEEN_TWO_NOTES
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CounterpointServiceTest extends AnyFlatSpec with Matchers with MockFactory {
  "Counterpoint service" should "get all notes between two notes when it does not have to add an octave" in {
    GET_ALL_NOTES_BETWEEN_TWO_NOTES("A#/Bb2", "G5") mustBe
      List(
        "A#/Bb2",
        "B2",
        "C3",
        "C#/Db3",
        "D3",
        "D#/Eb3",
        "E3",
        "F3",
        "F#/Gb3",
        "G3",
        "G#/Ab3",
        "A3",
        "A#/Bb3",
        "B3",
        "C4",
        "C#/Db4",
        "D4",
        "D#/Eb4",
        "E4",
        "F4",
        "F#/Gb4",
        "G4",
        "G#/Ab4",
        "A4",
        "A#/Bb4",
        "B4",
        "C5",
        "C#/Db5",
        "D5",
        "D#/Eb5",
        "E5",
        "F5",
        "F#/Gb5",
        "G5"
      )
  }
}
