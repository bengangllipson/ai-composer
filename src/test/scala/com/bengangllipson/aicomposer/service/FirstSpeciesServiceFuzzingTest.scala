package com.bengangllipson.aicomposer.service

import com.bengangllipson.aicomposer.service.CantusFirmusServiceFuzzingTest.MELODIC_CONSONANCES
import com.bengangllipson.aicomposer.service.CounterpointService.GET_ALL_NOTES_BETWEEN_TWO_NOTES
import com.bengangllipson.aicomposer.service.FirstSpeciesServiceFuzzingTest.{AVAILABLE_CANTUS_FIRMUS_NOTES, AVAILABLE_FIRST_SPECIES_NOTES, HARMONIC_CONSONANCES, TRIES}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.util.{Failure, Random, Success}


class FirstSpeciesServiceFuzzingTest extends AnyFlatSpec with Matchers with MockFactory {
  val randomService: RandomService = mock[RandomService]
  val counterpointService = new CounterpointService()

  def cantusFirmusService = new CantusFirmusService(randomService, counterpointService)
  def firstSpeciesService = new FirstSpeciesService(randomService, counterpointService)

  private def setUpCantusFirmus(tonic: Int, maxTonic: Int) = {
    val length = Random.between(8, 17)
    randomService.between.expects(8, 17).returning(length)
    randomService.between.expects(3, maxTonic).returning(tonic)
    (1 to 13).map(i => {
      val next = Random.nextInt(i)
      randomService.nextInt.expects(i).returning(next).anyNumberOfTimes()
    })
    (() => randomService.nextDouble()).expects().returning(Random.nextDouble()).anyNumberOfTimes()
  }

  private def setUpFirstSpecies() = {
    val next = Random.nextInt(14)
    randomService.nextInt.expects(14).returning(next).anyNumberOfTimes()
  }

  private def testWrapper(callback: (List[String], List[String]) => Unit) = {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUpCantusFirmus(tonic, maxTonic)
      setUpFirstSpecies()
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          firstSpeciesService.generate(cantusFirmus) match {
            case Success(firstSpecies) =>
              callback(cantusFirmus, firstSpecies)
            case Failure(e) =>
              e.printStackTrace()
              fail()
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  private def failTest(firstSpecies: List[String], cantusFirmus: List[String] = List()) = {
    println("FAILURE FOUND WITH THIS FIRST SPECIES:")
    println(firstSpecies.toString())
    if (cantusFirmus.nonEmpty) {
      val availableNotes = GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "A4")
      println("AGAINST THIS CANTUS FIRMUS:")
      println(cantusFirmus.toString())
      println("WITH THESE HARMONIC INTERVALS:")
      println(cantusFirmus.zipWithIndex.map {
        case (cfNote, i) =>
          math.abs(availableNotes.indexOf(cfNote) - availableNotes.indexOf(firstSpecies(i)))
      })
    }
    fail()
  }

  "First species service" should "generate a first species that's the same length as the cantus firmus" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      if (firstSpecies.length != cantusFirmus.length) {
        failTest(firstSpecies)
      }
    })
  }

  "First species service" should "ensure that all notes present in the first species are within the available notes" in {
    testWrapper((_, firstSpecies) => {
      firstSpecies.foreach(note => {
        if (!AVAILABLE_FIRST_SPECIES_NOTES.contains(note)) {
          failTest(firstSpecies)
        }
      })
    })
  }

  "First species service" should "generate a first species that starts with either do or sol" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      if (!List(0, 7, 12).contains(counterpointService.getInterval(cantusFirmus.head, firstSpecies.head, GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "A4")))) {
        failTest(firstSpecies)
      }
    })
  }

  "First species service" should "ensure that all notes are in the right major key" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      val notesInKey = counterpointService.getInMajorKeyNotes(cantusFirmus.head, AVAILABLE_FIRST_SPECIES_NOTES)
      if (firstSpecies.count(note => notesInKey.contains(note)) != firstSpecies.length) {
        failTest(firstSpecies)
      }
    })
  }

  "First species service" should "ensure that all note-to-note progressions are melodic consonances, " +
    "and consequentially that there are no leaps greater than an octave" in {
    testWrapper((_, firstSpecies) => {
      firstSpecies.zipWithIndex.map {
        case (note, i) =>
          if (i > 0) {
            if (!MELODIC_CONSONANCES.contains(
              math.abs(
                AVAILABLE_FIRST_SPECIES_NOTES.indexOf(note) -
                  AVAILABLE_FIRST_SPECIES_NOTES.indexOf(firstSpecies(i - 1))
              )
            )) {
              failTest(firstSpecies)
            }
          }
      }
    })
  }

  "First species service" should "ensure that all vertical intervals are harmonic consonances" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      firstSpecies.zipWithIndex.map {
        case (note, i) =>
          if (!HARMONIC_CONSONANCES.contains(
            math.abs(
              GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "A4").indexOf(note) -
                GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "A4").indexOf(cantusFirmus(i))
            )
          )) {
            failTest(firstSpecies, cantusFirmus)
          }
      }
    })
  }

  "First species service" should "ensure that no note is followed by the same note" in {
    testWrapper((_, firstSpecies) => {
      firstSpecies.zipWithIndex.map {
        case (note, i) =>
          if (i > 0) {
            if (note == firstSpecies(i - 1)) {
              failTest(firstSpecies)
            }
          }
      }
    })
  }

  "First species service" should "ensure that the leading tone always leads to the tonic" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      firstSpecies.zipWithIndex.map {
        case (note, i) =>
          if (
            AVAILABLE_FIRST_SPECIES_NOTES
              .filter(n =>
                n.filterNot(c => c.isDigit) == AVAILABLE_CANTUS_FIRMUS_NOTES(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.head) - 1).filterNot(c => c.isDigit)
              ).contains(note)) {
            def x = firstSpecies(i + 1)
            def y = AVAILABLE_FIRST_SPECIES_NOTES(AVAILABLE_FIRST_SPECIES_NOTES.indexOf(note) + 1)
            if (x != y) {
              failTest(firstSpecies, cantusFirmus)
            }
          }
      }
    })
  }

  "First species service" should "ensure that each leap greater than a 3rd is " +
    "followed by a stepwise motion in the opposite direction" in {
    testWrapper((_, firstSpecies) => {
      firstSpecies.zipWithIndex.map {
        case (note, i) =>
          if (i > 0 && i < firstSpecies.length - 1) {
            val noteIdx = AVAILABLE_FIRST_SPECIES_NOTES.indexOf(note)
            val prevNoteIdx = AVAILABLE_FIRST_SPECIES_NOTES.indexOf(firstSpecies(i - 1))
            val nextNoteIdx = AVAILABLE_FIRST_SPECIES_NOTES.indexOf(firstSpecies(i + 1))
            if ( math.abs(noteIdx - prevNoteIdx) > 4) {
              if (math.abs(nextNoteIdx - noteIdx) > 2) {
                failTest(firstSpecies)
              }
            }
          }
      }
    })
  }

  "First species service" should "ensure that there are no more than two leaps in a row" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      val notesInKey = counterpointService.getInMajorKeyNotes(AVAILABLE_CANTUS_FIRMUS_NOTES(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.head)), AVAILABLE_FIRST_SPECIES_NOTES)
      firstSpecies.zipWithIndex.map {
        case (note, i) =>
          if (i > 2) {
            val prevNote = firstSpecies(i - 1)
            val prevPrevNote = firstSpecies(i - 2)
            val prevPrevPrevNote = firstSpecies(i - 3)
            if (math.abs(notesInKey.indexOf(prevPrevPrevNote) - notesInKey.indexOf(prevPrevNote)) > 1) {
              if (math.abs(notesInKey.indexOf(prevPrevNote) - notesInKey.indexOf(prevNote)) > 1) {
                if (math.abs(notesInKey.indexOf(prevNote) - notesInKey.indexOf(note)) > 1) {
                  failTest(firstSpecies)
                }
                math.abs(notesInKey.indexOf(prevNote) - notesInKey.indexOf(note)) > 1 mustBe false
              }
            }
          }
      }
    })
  }

  "First species service" should "ensure that consecutive leaps don't go in the same direction" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      val notesInKey = counterpointService.getInMajorKeyNotes(AVAILABLE_CANTUS_FIRMUS_NOTES(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.head)), AVAILABLE_FIRST_SPECIES_NOTES)
      firstSpecies.zipWithIndex.map {
        case (note, i) =>
          if (i > 1) {
            val prevNote = firstSpecies(i - 1)
            val prevPrevNote = firstSpecies(i - 2)
            val firstLeapMinusVal = notesInKey.indexOf(prevPrevNote) - notesInKey.indexOf(prevNote)
            if (math.abs(firstLeapMinusVal) > 1) {
              val direction = if (firstLeapMinusVal > 0) {
                "down"
              } else {
                "up"
              }
              val secondLeapMinusVal = notesInKey.indexOf(prevNote) - notesInKey.indexOf(note)
              if (math.abs(secondLeapMinusVal) > 1) {
                if ((secondLeapMinusVal > 0 && direction == "down") || (secondLeapMinusVal < 0 && direction == "up")) {
                  failTest(firstSpecies)
                }
                if (secondLeapMinusVal > 0) {
                  direction mustBe "up"
                } else {
                  direction mustBe "down"
                }
              }
            }
          }
      }
    })
  }

  "First species service" should "ensure that there's one high point" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      val highestNote = firstSpecies.maxBy(note => AVAILABLE_FIRST_SPECIES_NOTES.indexOf(note))
      if (firstSpecies.count(note => note == highestNote) != 1 || !(firstSpecies.indexOf(highestNote) >= firstSpecies.length / 4) || !(firstSpecies.indexOf(highestNote) <= firstSpecies.length - (firstSpecies.length / 4))) {
        failTest(firstSpecies)
      }
    })
  }

  "First species service" should "ensure no repetition of motives or licks " in {
    testWrapper((cantusFirmus, firstSpecies) => {
      val notesInKey = counterpointService.getInMajorKeyNotes(AVAILABLE_CANTUS_FIRMUS_NOTES(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.head)), AVAILABLE_FIRST_SPECIES_NOTES)
      firstSpecies.zipWithIndex.foreach {
        case (note, i) =>
          if (i > 0) {
            val lastNote = firstSpecies(i - 1)
            val countOfRepetitions = firstSpecies.zipWithIndex.count {
              case (innerNote, j) => {
                val tonicNoOctave = cantusFirmus.head.filterNot(c => c.isDigit)
                val lt = notesInKey(notesInKey.map(note => note.filterNot(c => c.isDigit)).lastIndexOf(firstSpecies.head.filterNot(c => c.isDigit)) - 1)
                val ltNoOctave = lt.filterNot(c => c.isDigit)
                j > 0 && j < firstSpecies.length - 2 && innerNote == note && firstSpecies(j - 1) == lastNote &&
                  (innerNote.filterNot(c => c.isDigit) != tonicNoOctave && firstSpecies(j - 1).filterNot(c => c.isDigit) != ltNoOctave) &&
                  (innerNote.filterNot(c => c.isDigit) != ltNoOctave && firstSpecies(j - 1).filterNot(c => c.isDigit) != tonicNoOctave)
              }
            }
            if (countOfRepetitions > 1) {
              failTest(firstSpecies)
            }
            countOfRepetitions <= 1 mustBe true
          }
      }
    })
  }

  "First species service" should "not contain the same note more than four times" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      val noteAndCountSeq = firstSpecies.groupBy(identity).view.mapValues(_.size).toSeq
      if (noteAndCountSeq.exists(noteAndCount =>
        noteAndCount._2 > 4
      )) {
        failTest(firstSpecies, cantusFirmus)
      }
    })
  }

  "First species service" should "not contain parallel fifths" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      cantusFirmus.zipWithIndex.foreach {
        case (cfNote, i) =>
          val availableNotes = GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "A4")
          val cfNoteIdx = availableNotes.indexOf(cfNote)
          val fsNoteIdx = availableNotes.indexOf(firstSpecies(i))
          if (i > 0 && (cfNoteIdx == -1 || fsNoteIdx == -1 ||
            math.abs(availableNotes.indexOf(cfNote) - availableNotes.indexOf(firstSpecies(i))) == 7
            )) {
            val lastCfNoteIdx = availableNotes.indexOf(cantusFirmus(i - 1))
            val lastFsNoteIdx = availableNotes.indexOf(firstSpecies(i - 1))
            if (lastCfNoteIdx == -1 || lastFsNoteIdx == -1 ||
              math.abs(availableNotes.indexOf(cantusFirmus(i - 1)) - availableNotes.indexOf(firstSpecies(i - 1))) == 7
            ) {
              failTest(firstSpecies, cantusFirmus)
            }
          }
      }
    })
  }

  "First species service" should "ensure that the first species ends with do" in {
    testWrapper((cantusFirmus, firstSpecies) => {
      if (!List(0, 12).contains(counterpointService.getInterval(cantusFirmus.last, firstSpecies.last, GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "A4")))) {
        failTest(firstSpecies)
      }
    })
  }
}

object FirstSpeciesServiceFuzzingTest {
  private val AVAILABLE_CANTUS_FIRMUS_NOTES: List[String] = GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "E4")
  private val HARMONIC_CONSONANCES: Set[Int] = Set(
    0, 3, 4, 5, 7, 8, 9, 12
  )
  val AVAILABLE_FIRST_SPECIES_NOTES: List[String] = GET_ALL_NOTES_BETWEEN_TWO_NOTES("A2", "A4")
  val TEST_CANTUS_FIRMUS: List[String] = List("G2", "E2", "E3", "D3", "F #/ Gb3", "G3", "G2", "A2", "F #/ Gb2", "G2", "F#/Gb2", "G2")
  val TRIES = 10
}
