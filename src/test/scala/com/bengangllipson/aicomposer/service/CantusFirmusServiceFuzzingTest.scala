package com.bengangllipson.aicomposer.service

import com.bengangllipson.aicomposer.service.CantusFirmusServiceFuzzingTest.{AVAILABLE_CANTUS_FIRMUS_NOTES, MELODIC_CONSONANCES, TRIES}
import com.bengangllipson.aicomposer.service.CounterpointService.GET_ALL_NOTES_BETWEEN_TWO_NOTES
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.util.{Failure, Random, Success}

class CantusFirmusServiceFuzzingTest extends AnyFlatSpec with Matchers with MockFactory {
  val randomService: RandomService = mock[RandomService]
  val counterpointService = new CounterpointService()

  def cantusFirmusService = new CantusFirmusService(randomService, counterpointService)


  private def setUp(tonic: Int, maxTonic: Int) = {
    val length = Random.between(8, 17)
    randomService.between.expects(8, 17).returning(length)
    randomService.between.expects(3, maxTonic).returning(tonic)
    (1 to 13).map(i => {
      val next = Random.nextInt(i)
      randomService.nextInt.expects(i).returning(next).anyNumberOfTimes()
    })
    (() => randomService.nextDouble()).expects().returning(Random.nextDouble()).anyNumberOfTimes()
  }

  "Cantus firmus service" should "generate a cantus firmus that starts and ends with the tonic" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          val head = cantusFirmus.head
          val last = cantusFirmus.last
          if (head.filterNot(c => c.isDigit) != last.filterNot(c => c.isDigit)) {
            println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
            println(cantusFirmus.toString())
          }
          head.filterNot(c => c.isDigit) mustBe last.filterNot(c => c.isDigit)
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure the final note is approached by stepwise motion" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          if (!List(AVAILABLE_CANTUS_FIRMUS_NOTES(tonic - 1).filterNot(c => c.isDigit), AVAILABLE_CANTUS_FIRMUS_NOTES(tonic + 2).filterNot(c => c.isDigit)).contains(cantusFirmus(cantusFirmus.length - 2).filterNot(c => c.isDigit))) {
            println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
            println(cantusFirmus.toString())
          }
          List(AVAILABLE_CANTUS_FIRMUS_NOTES(tonic - 1).filterNot(c => c.isDigit), AVAILABLE_CANTUS_FIRMUS_NOTES(tonic + 2).filterNot(c => c.isDigit)).contains(cantusFirmus(cantusFirmus.length - 2).filterNot(c => c.isDigit)) mustBe true
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "always approach the final note by the leading tone if the third to last note is the 2" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)

      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          if (cantusFirmus(cantusFirmus.length - 3) == AVAILABLE_CANTUS_FIRMUS_NOTES(tonic + 2)) {
            if (cantusFirmus(cantusFirmus.length - 2) != AVAILABLE_CANTUS_FIRMUS_NOTES(tonic - 1)) {
              println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
              println(cantusFirmus.toString())
            }
            cantusFirmus(cantusFirmus.length - 2) mustBe AVAILABLE_CANTUS_FIRMUS_NOTES(tonic - 1)
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that no note is followed by the same note" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.map {
            case (note, i) =>
              if (i > 0) {
                if (note == cantusFirmus(i - 1)) {
                  println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                  println(cantusFirmus.toString())
                }
                note must not equal cantusFirmus(i - 1)
              }
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that all notes are in the right major key" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      val notesInKey = counterpointService.getInMajorKeyNotes(AVAILABLE_CANTUS_FIRMUS_NOTES(tonic), AVAILABLE_CANTUS_FIRMUS_NOTES)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          if (cantusFirmus.count(note => notesInKey.contains(note)) != cantusFirmus.length) {
            println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
            println(cantusFirmus.toString())
          }
          cantusFirmus.count(note => notesInKey.contains(note)) mustBe cantusFirmus.length
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that the third to last note is not the leading tone" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          if (AVAILABLE_CANTUS_FIRMUS_NOTES
            .filter(note =>
              note.filterNot(c => c.isDigit) == AVAILABLE_CANTUS_FIRMUS_NOTES(tonic - 1).filterNot(c => c.isDigit)
            ).contains(cantusFirmus(cantusFirmus.length - 3))) {
            println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
            println(cantusFirmus.toString())
          }
          AVAILABLE_CANTUS_FIRMUS_NOTES
            .filter(note =>
              note.filterNot(c => c.isDigit) == AVAILABLE_CANTUS_FIRMUS_NOTES(tonic - 1).filterNot(c => c.isDigit)
            ).contains(cantusFirmus(cantusFirmus.length - 3)) mustBe false
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that the leading tone always leads to the tonic" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.map {
            case (note, i) =>
              if (AVAILABLE_CANTUS_FIRMUS_NOTES
                .filter(note =>
                  note.filterNot(c => c.isDigit) == AVAILABLE_CANTUS_FIRMUS_NOTES(tonic - 1).filterNot(c => c.isDigit)
                ).contains(note)) {
                if (cantusFirmus(i + 1) != AVAILABLE_CANTUS_FIRMUS_NOTES(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) + 1)) {
                  println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                  println(cantusFirmus.toString())
                }
                cantusFirmus(i + 1) mustBe AVAILABLE_CANTUS_FIRMUS_NOTES(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) + 1)
              }
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that all note-to-note progressions are melodic consonances and ensure that no leaps are greater than an octave" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.map {
            case (note, i) =>
              if (i > 0) {
                if (!MELODIC_CONSONANCES.contains(
                  math.abs(
                    AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) -
                      AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus(i - 1))
                  )
                )) {
                  println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                  println(cantusFirmus.toString())
                }
                MELODIC_CONSONANCES.contains(
                  math.abs(
                    AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) -
                      AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus(i - 1))
                  )
                ) mustBe true
              }
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that the range between the lowest note and the highest note is no larger than a tenth" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          val stepwiseValues = cantusFirmus.map(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note))
          if (stepwiseValues.max - stepwiseValues.min > 16) {
            println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
            println(cantusFirmus.toString())
          }
          stepwiseValues.max - stepwiseValues.min <= 16 mustBe true
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that each leap greater than a 3rd is " +
    "followed by a stepwise motion in the opposite direction" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.map {
            case (note, i) =>
              if (i > 0 && i < cantusFirmus.length - 1) {
                val noteIdx = AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)
                val prevNoteIdx = AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus(i - 1))
                val nextNoteIdx = AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus(i + 1))
                if (math.abs(noteIdx - prevNoteIdx) > 4) {
                  if (math.abs(nextNoteIdx - noteIdx) > 2) {
                    println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                    println(cantusFirmus.toString())
                  }
                  math.abs(nextNoteIdx - noteIdx) <= 2 mustBe true
                }
              }
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that there are no more than two leaps in a row" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      val notesInKey = counterpointService.getInMajorKeyNotes(AVAILABLE_CANTUS_FIRMUS_NOTES(tonic), AVAILABLE_CANTUS_FIRMUS_NOTES)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.map {
            case (note, i) =>
              if (i > 2) {
                val prevNote = cantusFirmus(i - 1)
                val prevPrevNote = cantusFirmus(i - 2)
                val prevPrevPrevNote = cantusFirmus(i - 3)
                if (math.abs(notesInKey.indexOf(prevPrevPrevNote) - notesInKey.indexOf(prevPrevNote)) > 1) {
                  if (math.abs(notesInKey.indexOf(prevPrevNote) - notesInKey.indexOf(prevNote)) > 1) {
                    if (math.abs(notesInKey.indexOf(prevNote) - notesInKey.indexOf(note)) > 1) {
                      println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                      println(cantusFirmus.toString())
                    }
                    math.abs(notesInKey.indexOf(prevNote) - notesInKey.indexOf(note)) > 1 mustBe false
                  }
                }
              }
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that consecutive leaps don't go in the same direction" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      val notesInKey = counterpointService.getInMajorKeyNotes(AVAILABLE_CANTUS_FIRMUS_NOTES(tonic), AVAILABLE_CANTUS_FIRMUS_NOTES)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.map {
            case (note, i) =>
              if (i > 1) {
                val prevNote = cantusFirmus(i - 1)
                val prevPrevNote = cantusFirmus(i - 2)
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
                      println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                      println(cantusFirmus.toString())
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
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure that there's one high point and that it's near the middle" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          val highestNote = cantusFirmus.maxBy(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note))
          if (cantusFirmus.count(note => note == highestNote) != 1 || !(cantusFirmus.indexOf(highestNote) >= cantusFirmus.length / 4) || !(cantusFirmus.indexOf(highestNote) <= cantusFirmus.length - (cantusFirmus.length / 4))) {
            println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
            println(cantusFirmus.toString())
          }
          cantusFirmus.count(note => note == highestNote) mustBe 1
          cantusFirmus.indexOf(highestNote) >= cantusFirmus.length / 4 mustBe true
          cantusFirmus.indexOf(highestNote) <= cantusFirmus.length - (cantusFirmus.length / 4) mustBe true
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "ensure no repetition of motives or licks" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      val notesInKey = counterpointService.getInMajorKeyNotes(AVAILABLE_CANTUS_FIRMUS_NOTES(tonic), AVAILABLE_CANTUS_FIRMUS_NOTES)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.foreach {
            case (note, i) =>
              if (i > 0) {
                val lastNote = cantusFirmus(i - 1)
                val countOfRepetitions = cantusFirmus.zipWithIndex.count {
                  case (innerNote, j) => {
                    val tonicNoOctave = cantusFirmus.head.filterNot(c => c.isDigit)
                    val ltNoOctave = notesInKey(notesInKey.indexOf(cantusFirmus.head) - 1).filterNot(c => c.isDigit)
                    j > 0 && j < cantusFirmus.length - 2 && innerNote == note && cantusFirmus(j - 1) == lastNote &&
                      (innerNote.filterNot(c => c.isDigit) != tonicNoOctave && cantusFirmus(j - 1).filterNot(c => c.isDigit) != ltNoOctave) &&
                      (innerNote.filterNot(c => c.isDigit) != ltNoOctave && cantusFirmus(j - 1).filterNot(c => c.isDigit) != tonicNoOctave)
                  }
                }
                if (countOfRepetitions > 1) {
                  println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                  println(cantusFirmus.toString())
                }
                countOfRepetitions <= 1 mustBe true
              }
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "not contain the same note more than twice other than the tonic at the start and end" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.groupBy(identity).view.mapValues(_.size)
            .toSeq.exists(noteAndCount => noteAndCount._1 != AVAILABLE_CANTUS_FIRMUS_NOTES(tonic) && noteAndCount._2 > 2) mustBe false
          cantusFirmus.groupBy(identity).view.mapValues(_.size)
            .toSeq.exists(noteAndCount => noteAndCount._1 == AVAILABLE_CANTUS_FIRMUS_NOTES(tonic) && noteAndCount._2 > 4) mustBe false
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }

  "Cantus firmus service" should "not contain the tonic more than once in the first half" in {
    (1 to TRIES).map(_ => {
      val maxTonic = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
      val tonic = Random.between(3, maxTonic)
      setUp(tonic, maxTonic)
      cantusFirmusService.generate() match {
        case Success(cantusFirmus) =>
          cantusFirmus.zipWithIndex.foreach {
            case (_, i) =>
              if (i > 0) {
                val countOfRepetitions = cantusFirmus.zipWithIndex.count {
                  case (innerNote, j) =>
                    j > 0 && j < cantusFirmus.length / 2 && innerNote == cantusFirmus.head
                }
                if (countOfRepetitions > 1) {
                  println("FAILURE FOUND WITH THIS CANTUS FIRMUS:")
                  println(cantusFirmus.toString())
                }
                countOfRepetitions <= 1 mustBe true
              }
          }
        case Failure(e) =>
          e.printStackTrace()
          fail()
      }
    })
  }
}


object CantusFirmusServiceFuzzingTest {
  private val AVAILABLE_CANTUS_FIRMUS_NOTES: List[String] = GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "E4")
  val MELODIC_CONSONANCES: Set[Int] = Set(
    1, 2, 3, 4, 5, 7, 8, 9, 12
  )
  val MAJOR_KEY_NOTES: Set[List[String]] = Set(
    List("E2", "F#/Gb2", "G#/Ab2", "A2", "B2", "C#/Db3", "D#/Eb3", "E3", "F#/Gb3", "G#/Ab3", "A3", "B3", "C#/Db4", "D#/Eb4"),
    List("F2", "G2", "A2", "A#/Bb2", "C3", "D3", "E3", "F3", "G3", "A3", "A#/Bb3", "C4", "D4", "E2"),
    List("F#/Gb2", "G#/Ab2", "A#/Bb2", "B2", "C#/Db3", "D#/Eb3", "F3", "F#/Gb3", "G#/Ab3", "A#/Bb3", "B3", "C#/Db4", "D#/Eb4", "F2"),
    List("G2", "A2", "B2", "C3", "D3", "E3", "F#/Gb3", "G3", "A3", "B3", "C4", "D4", "E2", "F#/Gb2"),
    List("G#/Ab2", "A#/Bb2", "C3", "C#/Db3", "D#/Eb3", "F3", "G3", "G#/Ab3", "A#/Bb3", "C4", "C#/Db4", "D#/Eb4", "F2", "G2"),
    List("A2", "B2", "C#/Db3", "D3", "E3", "F#/Gb3", "G#/Ab3", "A3", "B3", "C#/Db4", "D4", "E2", "F#/Gb2", "G#/Ab2"),
    List("A#/Bb2", "C3", "D3", "D#/Eb3", "F3", "G3", "A3", "A#/Bb3", "C4", "D4", "D#/Eb4", "F2", "G2", "A2"),
    List("B2", "C#/Db3", "D#/Eb3", "E3", "F#/Gb3", "G#/Ab3", "A#/Bb3", "B3", "C#/Db4", "D#/Eb4", "E2", "F#/Gb2", "G#/Ab2", "A#/Bb2"),
    List("C3", "D3", "E3", "F3", "G3", "A3", "B3", "C4", "D4", "E2", "F2", "G2", "A2", "B2"),
    List("C#/Db3", "D#/Eb3", "F3", "F#/Gb3", "G#/Ab3", "A#/Bb3", "C4", "C#/Db4", "D#/Eb4", "F2", "F#/Gb2", "G#/Ab2", "A#/Bb2", "C3"),
    List("D3", "E3", "F#/Gb3", "G3", "A3", "B3", "C#/Db4", "D4", "E2", "F#/Gb2", "G2", "A2", "B2", "C#/Db3"),
    List("D#/Eb3", "F3", "G3", "G#/Ab3", "A#/Bb3", "C4", "D4", "D#/Eb4", "F2", "G2", "G#/Ab2", "A#/Bb2", "C3", "D3"),
  )

  val TRIES = 100
}