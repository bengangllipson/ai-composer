package com.bengangllipson.aicomposer

import com.bengangllipson.aicomposer.CantusFirmusService.{AVAILABLE_CANTUS_FIRMUS_NOTES, MAJOR_KEY_INTERVALS, MAX_LENGTH, MAX_TONIC, MELODIC_CONSONANCES, MIN_LENGTH, MIN_TONIC, OCTAVE}
import com.bengangllipson.aicomposer.CounterpointService.NOTES

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

private class CantusFirmusService(var randomService: RandomService, var counterpointService: CounterpointService) extends Counterpoint {
  def this() = {
    this(new RandomService(), new CounterpointService())
  }

  override def generate(cantusFirmus: List[String] = List()): Try[List[String]] = Try {
    val length = randomService.between(MIN_LENGTH, MAX_LENGTH + 1)
    val tonic = AVAILABLE_CANTUS_FIRMUS_NOTES(randomService.between(MIN_TONIC, MAX_TONIC))
    val inMajorKeyCantusFirmusNotes = getInMajorKeyCantusFirmusNotes(tonic)
    val cantusFirmusDepthFirst = generateCantusFirmusDepthFirst(length, tonic, inMajorKeyCantusFirmusNotes)
    if (cantusFirmusDepthFirst.nonEmpty) {
      cantusFirmusDepthFirst
    } else {
      throw new Exception("Could not generate cantus firmus.")
    }
  }

  override def formatOutput(cantusFirmus: List[String]): List[String] = {
    counterpointService.formatOutput(cantusFirmus)
  }

  @tailrec
  private def generateCantusFirmusDepthFirst(length: Int, tonic: String, inMajorKeyNotes: List[String], cantusFirmus: List[String] = List(), invalidLines: List[List[String]] = List(), invalidNotePos: Int = -1): List[String] = {
    if (cantusFirmus.length == length) {
      cantusFirmus
    } else {
      generateCantusFirmusNote(length, tonic, inMajorKeyNotes, cantusFirmus, invalidLines) match {
        case Success(nextNote) =>
          generateCantusFirmusDepthFirst(length, tonic, inMajorKeyNotes, cantusFirmus :+ nextNote, invalidLines, invalidNotePos)
        case Failure(invalidNoteException) =>
          val invalidNoteMessage = invalidNoteException.getMessage
          if (invalidNoteMessage == "Can not generate cantus firmus.") {
            cantusFirmus
          } else {
            generateCantusFirmusDepthFirst(length, tonic, inMajorKeyNotes, cantusFirmus.dropRight(1), invalidLines :+ invalidNoteMessage.filter(c => !"List()".contains(c)).replace(" ", "").split(",").toList, invalidNoteMessage.last.toInt)
          }
      }
    }
  }

  private def applyPreferenceRules(notes: Seq[String], cantusFirmus: List[String]): Seq[String] = {
    // todo: maybe no leaps in the first few notes?
    // todo: step declination. It should ascend more than it descends.
    // todo: melodic arch would help make step declination happen.
    // todo: step inertia? kinda happens already.
    val smallLeapsOrStepwiseAvailable = notes.exists(note => !isALargeLeap(note, cantusFirmus.last))
    if (smallLeapsOrStepwiseAvailable) {
      notes.filter(note => !isALargeLeap(note, cantusFirmus.last))
    } else {
      notes
    }
  }

  private def generateCantusFirmusNote(length: Int, tonic: String, inMajorKeyNotes: List[String], cantusFirmus: List[String], invalidLines: List[List[String]]): Try[String] = {
    if (isFirstNote(cantusFirmus)) {
      if (invalidLines.exists(line => line.length == 1)) {
        Failure(new Exception("Can not generate cantus firmus."))
      } else {
        Success(tonic)
      }
    } else {
      val universalRulesApplied = applyUniversalRules(inMajorKeyNotes, cantusFirmus, invalidLines, length)
      val leapsRulesApplied = applyLeapsRules(inMajorKeyNotes, cantusFirmus, universalRulesApplied)
      val individualRulesApplied = applyIndividualRules(inMajorKeyNotes, length, cantusFirmus, tonic, leapsRulesApplied)
      val preferenceRulesApplied = applyPreferenceRules(individualRulesApplied, cantusFirmus)
      val availableNotes = preferenceRulesApplied

      if (availableNotes.isEmpty) {
        Failure(new Exception(s"$cantusFirmus"))
      } else {
        Success(availableNotes(randomService.nextInt(availableNotes.length)))
      }
    }
  }

  private def getNoteForInterval(tonic: String, interval: Int) =
    AVAILABLE_CANTUS_FIRMUS_NOTES(
      (AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(tonic) + interval) %
        (OCTAVE * 2)
    )

  private def intervalIsInMajorKey(interval: Int) =
    MAJOR_KEY_INTERVALS.contains(interval % OCTAVE)

  private def noteIsInMajorKey(tonic: String, note: String, majorKeyIntervals: Seq[Int]) =
    majorKeyIntervals
      .map(interval => {
        getNoteForInterval(tonic, interval)
      })
      .contains(note)

  def getInMajorKeyCantusFirmusNotes(tonic: String): List[String] =
    AVAILABLE_CANTUS_FIRMUS_NOTES.filter(note => {
      noteIsInMajorKey(tonic, note, AVAILABLE_CANTUS_FIRMUS_NOTES.indices
        .filter(interval => intervalIsInMajorKey(interval)))
    })

  private def isLastNote(length: Int, cantusFirmus: List[String]) = {
    cantusFirmus.length == length - 1
  }

  private def isFirstNote(cantusFirmus: List[String]) = {
    cantusFirmus.isEmpty
  }

  private def applyIndividualRules(inMajorKeyNotes: Seq[String], length: Int, cantusFirmus: List[String], tonic: String, notes: Seq[String]) = {
    val climaxMustBeInMiddleApplied = applyClimaxMustBeInMiddleRule(cantusFirmus, length, notes)
    val notePairs =
      cantusFirmus.zipWithIndex.map {
        case (note, i) =>
          if (i > 0) {
            (cantusFirmus(i - 1), note)
          } else {
            ("", "")
          }
      }.filter(pair => pair._1 != "" && pair._2 != "")

    val noMotivesApplied = if (!isPenultimateNote(length, cantusFirmus) && !isLastNote(length, cantusFirmus)) {
      climaxMustBeInMiddleApplied.filter(note => !notePairs.contains((cantusFirmus.last, note)))
    } else {
      climaxMustBeInMiddleApplied
    }


    if (isLastNote(length, cantusFirmus)) {
      noMotivesApplied.filter(note => applyFinalNoteAsTonicRule(inMajorKeyNotes, cantusFirmus, tonic, note))
    } else if (isPenultimateNote(length, cantusFirmus)) {
      noMotivesApplied.filter(note => applyPenultimateStepwiseMotionRule(inMajorKeyNotes, tonic).contains(note))
    } else if (isLeadingTone(inMajorKeyNotes, cantusFirmus, tonic)) {
      noMotivesApplied.filter(note => applyLeadingToneLeadsToTonicRule(cantusFirmus, note))
    } else if (isAntePenultimateNote(length, cantusFirmus)) {
      noMotivesApplied.filter(note => applyAntePenultimateCannotBeLeadingToneRule(inMajorKeyNotes, tonic, note))
    } else {
      noMotivesApplied
    }
  }

  private def applyClimaxMustBeInMiddleRule(cantusFirmus: List[String], length: Int, notes: Seq[String]) = {
    if (isInBackQuarter(cantusFirmus, length)) {
      notes.filter(note => noteIsLowerThanClimax(cantusFirmus, note))
    } else if (isInFirstQuarter(cantusFirmus, length)) {
      notes.filter(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) < 19)
    } else if (isTheNoteBeforeBackQuarter(cantusFirmus, length) && climaxIsInTheBeginning(cantusFirmus, length)) {
      notes.filter(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) > getMaxNote(cantusFirmus))
    } else if (isInMiddle(cantusFirmus, length) && climaxIsInTheBeginning(cantusFirmus, length)) {
      notes.filter(note => directionIsUpwards(cantusFirmus, note))
    } else {
      notes
    }
  }

  // perhaps give it the reason whenever it fails. like a list of pairs [invalidLine to reasonItFailed]
  // then if the reason is because the climax was at the beginning, only pick notes that are higher than
  // the note picked before in that position.

  private def directionIsUpwards(cantusFirmus: List[String], note: String) = {
    AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) > AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.last)
  }

  private def climaxIsInTheBeginning(cantusFirmus: List[String], length: Int) = {
    cantusFirmus.filter(note => noteIsInFirstThird(note, cantusFirmus, length)).map(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)).contains(getMaxNote(cantusFirmus))
  }

  private def noteIsLowerThanClimax(cantusFirmus: List[String], note: String) = {
    AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) < getMaxNote(cantusFirmus)
  }

  private def getMaxNote(cantusFirmus: List[String]) = {
    cantusFirmus.map(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)).max
  }

  private def isInBackQuarter(cantusFirmus: List[String], length: Int) = {
    cantusFirmus.length >= (length - (length / 4))
  }

  private def isTheNoteBeforeBackQuarter(cantusFirmus: List[String], length: Int) = {
    cantusFirmus.length == (length - (length / 4)) - 1
  }

  private def isInFirstQuarter(cantusFirmus: List[String], length: Int) = {
    cantusFirmus.length <= (length / 4)
  }

  private def isInMiddle(cantusFirmus: List[String], length: Int) = {
    cantusFirmus.length < (length / 4) && cantusFirmus.length < (length - (length / 4))
  }

  private def noteIsInFirstThird(note: String, cantusFirmus: List[String], length: Int) = {
    cantusFirmus.indexOf(note) < (length / 3)
  }

  private def applyFinalNoteAsTonicRule(inMajorKeyNotes: Seq[String], cantusFirmus: List[String], tonic: String, note: String) =
    (note.filterNot(c => c.isDigit) == tonic.filterNot(c => c.isDigit)) && (math.abs(inMajorKeyNotes.indexOf(cantusFirmus.last) - inMajorKeyNotes.indexOf(note)) == 1)

  private def applyAntePenultimateCannotBeLeadingToneRule(inMajorKeyNotes: Seq[String], tonic: String, note: String) =
    note.filterNot(c => c.isDigit) != inMajorKeyNotes(inMajorKeyNotes.indexOf(tonic) - 1).filterNot(c => c.isDigit)

  private def isAntePenultimateNote(length: Int, cantusFirmus: List[String]) =
    cantusFirmus.length == length - 3

  private def isLeadingTone(inMajorKeyNotes: Seq[String], cantusFirmus: List[String], tonic: String) =
    cantusFirmus.last.filterNot(c => c.isDigit) == inMajorKeyNotes(inMajorKeyNotes.indexOf(tonic) - 1).filterNot(c => c.isDigit)

  private def applyLeadingToneLeadsToTonicRule(cantusFirmus: List[String], note: String) =
    AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) - AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.last) == 1

  private def applyPenultimateStepwiseMotionRule(inMajorKeyNotes: Seq[String], tonic: String) =
    inMajorKeyNotes.filter(note => {
      // todo: if the fourth to last note is the LT use the 2
      val noteWithoutOctave = note.filterNot(c => c.isDigit)
      noteWithoutOctave == inMajorKeyNotes(inMajorKeyNotes.indexOf(tonic) - 1).filterNot(c => c.isDigit) ||
        noteWithoutOctave == inMajorKeyNotes(inMajorKeyNotes.indexOf(tonic) + 1).filterNot(c => c.isDigit)
    })

  private def isPenultimateNote(length: Int, cantusFirmus: List[String]) =
    cantusFirmus.length == length - 2

  private def applyLeapsRules(inMajorKeyNotes: List[String], cantusFirmus: List[String], notes: Seq[String]) =
    if (followingALargeLeap(cantusFirmus)) {
      val next = if (isDownwardsMotion(cantusFirmus)) {
        inMajorKeyNotes(inMajorKeyNotes.indexOf(cantusFirmus.last) + 1)
      } else {
        inMajorKeyNotes(inMajorKeyNotes.indexOf(cantusFirmus.last) - 1)
      }
      if (notes.contains(next)) {
        Seq(next)
      } else {
        Seq()
      }
    } else if (followingALeap(cantusFirmus, inMajorKeyNotes)) {
      if (followingTwoLeaps(cantusFirmus, inMajorKeyNotes)) {
        notes.filter(note => !isALeap(note, cantusFirmus.last, inMajorKeyNotes))
      } else {
        val previousLeapDirection = if (inMajorKeyNotes.indexOf(cantusFirmus(cantusFirmus.length - 2)) - inMajorKeyNotes.indexOf(cantusFirmus.last) > 0) {
          "down"
        } else {
          "up"
        }
        notes.filter(note => {
          val secondLeapDirection = if (inMajorKeyNotes.indexOf(cantusFirmus.last) - inMajorKeyNotes.indexOf(note) > 0) {
            "down"
          } else {
            "up"
          }
          !isALeap(note, cantusFirmus.last, inMajorKeyNotes) || previousLeapDirection != secondLeapDirection
        })
      }
    } else {
      notes
    }

  private def isDownwardsMotion(cantusFirmus: List[String]) =
    AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus(cantusFirmus.length - 2)) - AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.last) > 0

  private def followingALargeLeap(cantusFirmus: List[String]) =
    cantusFirmus.length > 1 &&
      math.abs(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus.last) - AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmus(cantusFirmus.length - 2))) >= 5

  private def followingALeap(cantusFirmus: List[String], inMajorKeyNotes: List[String]) =
    cantusFirmus.length > 1 &&
      isALeap(cantusFirmus.last, cantusFirmus(cantusFirmus.length - 2), inMajorKeyNotes)

  private def followingTwoLeaps(cantusFirmus: List[String], inMajorKeyNotes: List[String]) =
    cantusFirmus.length > 2 &&
      isALeap(cantusFirmus(cantusFirmus.length - 2), cantusFirmus(cantusFirmus.length - 3), inMajorKeyNotes) &&
      isALeap(cantusFirmus.last, cantusFirmus(cantusFirmus.length - 2), inMajorKeyNotes)

  private def isALeap(note: String, prevNote: String, inMajorKeyNotes: List[String]): Boolean =
    math.abs(inMajorKeyNotes.indexOf(note) - inMajorKeyNotes.indexOf(prevNote)) > 1

  private def isALargeLeap(note: String, prevNote: String) =
    math.abs(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) - AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(prevNote)) >= 5

  private def applyUniversalRules(notes: Seq[String], cantusFirmus: List[String], invalidLines: List[List[String]], length: Int) = {
    val lowestNote = cantusFirmus.map(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)).min
    val highestNote = cantusFirmus.map(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)).max
    val countsOfNotes = cantusFirmus.groupBy(identity).view.mapValues(_.size).toSeq
    notes
      .filter(note => {
        val noteIdx = AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)
        val ret = applyNoRepeatedNotesRule(cantusFirmus, note) &&
          applyMaxRangeRule(lowestNote, highestNote, noteIdx) &&
          !invalidLines.contains(cantusFirmus :+ note) &&
          // todo: it can equal the max note if it keeps going higher than it later though
          note != cantusFirmus.maxBy(cantusFirmusNote => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(cantusFirmusNote)) &&
          isMelodicConsonance(cantusFirmus.last, note)
        applyMaxRepetitionRules(cantusFirmus, countsOfNotes, note, ret, length)
      })
  }

  private def applyMaxRepetitionRules(cantusFirmus: List[String], countsOfNotes: Seq[(String, Int)], note: String, ret: Boolean, length: Int) = {
    if (noteIsTonic(cantusFirmus, note) && noteHasOccurredPreviously(countsOfNotes, note)) {
      applyTonicMaxRepetitionRule(countsOfNotes, note, ret, cantusFirmus.length < length / 2)
    } else if (noteHasOccurredPreviously(countsOfNotes, note)) {
      applyMaxRepetitionRule(countsOfNotes, note, ret)
    } else {
      ret
    }
  }

  private def applyMaxRepetitionRule(countsOfNotes: Seq[(String, Int)], note: String, ret: Boolean) = {
    ret && countsOfNotes.filter(can => can._1 == note).head._2 < 2
  }

  private def applyTonicMaxRepetitionRule(countsOfNotes: Seq[(String, Int)], note: String, ret: Boolean, isInFirstHalf: Boolean) = {
    if (isInFirstHalf) {
      false
    } else {
      ret && countsOfNotes.filter(can => can._1 == note).head._2 < 4
    }
  }

  private def noteIsTonic(cantusFirmus: List[String], note: String) = {
    note == cantusFirmus.head
  }

  private def noteHasOccurredPreviously(countsOfNotes: Seq[(String, Int)], note: String) = {
    countsOfNotes.map(nac => nac._1).contains(note)
  }

  private def applyMaxRangeRule(lowestNote: Int, highestNote: Int, noteIdx: Int) = {
    noteIdx - lowestNote <= 16 && highestNote - noteIdx <= 16
  }

  private def applyNoRepeatedNotesRule(cantusFirmus: List[String], note: String) = {
    note != cantusFirmus.last
  }

  private def isMelodicConsonance(lastNote: String, note: String) = {
    MELODIC_CONSONANCES
      .contains(math.abs(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(lastNote) - AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)))
  }
}

private object CantusFirmusService {
  val OCTAVE = 12
  val MIN_LENGTH = 8
  val MAX_LENGTH = 16

  val MAJOR_KEY_INTERVALS: Set[Int] = Set(
    0, 2, 4, 5, 7, 9, 11
  )

  val AVAILABLE_CANTUS_FIRMUS_NOTES: List[String] = GENERATE_AVAILABLE_CANTUS_FIRMUS_NOTES(2, 2)
  val MIN_TONIC: Int = 3
  val MAX_TONIC: Int = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7

  def GENERATE_AVAILABLE_CANTUS_FIRMUS_NOTES(numOctaves: Int, startOctave: Int): List[String] =
    (0 until NOTES.length * numOctaves).map(noteIdx => {
      val stepsAboveC = OCTAVE - NOTES.slice(NOTES.length - NOTES.indexOf("C"), NOTES.length).length
      val currentOctave = startOctave + math.floor((noteIdx + stepsAboveC) / OCTAVE).toInt
      NOTES(noteIdx % NOTES.length).concat(currentOctave.toString)
    }).toList

  def MELODIC_CONSONANCES: Set[Int] = Set(
    1, 2, 3, 4, 5, 7, 8, 9, 12
  )
}
