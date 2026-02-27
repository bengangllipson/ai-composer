package com.bengangllipson.aicomposer.service

import com.bengangllipson.aicomposer.service.CantusFirmusService.*
import CounterpointService.{GET_ALL_NOTES_BETWEEN_TWO_NOTES, MELODIC_CONSONANCES}
import com.bengangllipson.aicomposer.service.{Counterpoint, CounterpointService, RandomService}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

private[aicomposer] class CantusFirmusService(var randomService: RandomService, var counterpointService: CounterpointService) extends Counterpoint {
  def this() = {
    this(new RandomService(), new CounterpointService())
  }

  override def generate(cantusFirmus: List[String] = List()): Try[List[String]] = Try {
    val length = randomService.between(MIN_LENGTH, MAX_LENGTH + 1)
    val tonic = AVAILABLE_CANTUS_FIRMUS_NOTES(randomService.between(MIN_TONIC, MAX_TONIC))
    val inMajorKeyCantusFirmusNotes = counterpointService.getInMajorKeyNotes(tonic, AVAILABLE_CANTUS_FIRMUS_NOTES)
    val cantusFirmus = generateCantusFirmusRecursive(length, tonic, inMajorKeyCantusFirmusNotes)
    if (cantusFirmus.nonEmpty) {
      cantusFirmus
    } else {
      throw new Exception("Could not generate cantus firmus.")
    }
  }

  override def formatOutput(cantusFirmus: List[String]): List[String] = {
    counterpointService.formatOutput(cantusFirmus)
  }

  @tailrec
  private def generateCantusFirmusRecursive(length: Int, tonic: String, inMajorKeyNotes: List[String], cantusFirmus: List[String] = List(), invalidLines: List[List[String]] = List(), invalidNotePos: Int = -1): List[String] = {
    if (cantusFirmus.length == length) {
      cantusFirmus
    } else {
      generateCantusFirmusNote(length, tonic, inMajorKeyNotes, cantusFirmus, invalidLines) match {
        case Success(nextNote) =>
          generateCantusFirmusRecursive(length, tonic, inMajorKeyNotes, cantusFirmus :+ nextNote, invalidLines, invalidNotePos)
        case Failure(invalidNoteException) =>
          val invalidNoteMessage = invalidNoteException.getMessage
          if (invalidNoteMessage == "Can not generate cantus firmus.") {
            cantusFirmus
          } else {
            generateCantusFirmusRecursive(length, tonic, inMajorKeyNotes, cantusFirmus.dropRight(1), invalidLines :+ invalidNoteMessage.filter(c => !"List()".contains(c)).replace(" ", "").split(",").toList, invalidNoteMessage.last.toInt)
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
    if (counterpointService.isFirstNote(cantusFirmus)) {
      if (invalidLines.exists(line => line.length == 1)) {
        Failure(new Exception("Can not generate cantus firmus."))
      } else {
        Success(tonic)
      }
    } else {
      val universalRulesApplied = applyUniversalRules(inMajorKeyNotes, cantusFirmus, invalidLines, length)
      val leapsRulesApplied = counterpointService.applyLeapsRules(inMajorKeyNotes, AVAILABLE_CANTUS_FIRMUS_NOTES, cantusFirmus, universalRulesApplied)
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

  private def applyIndividualRules(inMajorKeyNotes: Seq[String], length: Int, cantusFirmus: List[String], tonic: String, notes: Seq[String]) = {
    val climaxMustBeInMiddleApplied = counterpointService.applyClimaxMustBeInMiddleRule(cantusFirmus, AVAILABLE_CANTUS_FIRMUS_NOTES, length, notes)
    val notePairs =
      cantusFirmus.zipWithIndex.map {
        case (note, i) =>
          if (i > 0) {
            (cantusFirmus(i - 1), note)
          } else {
            ("", "")
          }
      }.filter(pair => pair._1 != "" && pair._2 != "")

    val noMotivesApplied = if (!counterpointService.isPenultimateNote(length, cantusFirmus) && !counterpointService.isLastNote(length, cantusFirmus)) {
      climaxMustBeInMiddleApplied.filter(note => !notePairs.contains((cantusFirmus.last, note)))
    } else {
      climaxMustBeInMiddleApplied
    }


    if (counterpointService.isLastNote(length, cantusFirmus)) {
      noMotivesApplied.filter(note => applyFinalNoteAsTonicRule(inMajorKeyNotes, cantusFirmus, tonic, note))
    } else if (counterpointService.isPenultimateNote(length, cantusFirmus)) {
      noMotivesApplied.filter(note => counterpointService.applyPenultimateStepwiseMotionRule(inMajorKeyNotes, tonic).contains(note))
    } else if (counterpointService.isLeadingTone(AVAILABLE_CANTUS_FIRMUS_NOTES, cantusFirmus, tonic)) {
      noMotivesApplied.filter(note => counterpointService.applyLeadingToneLeadsToTonicRule(cantusFirmus, note, AVAILABLE_CANTUS_FIRMUS_NOTES))
    } else if (counterpointService.isAntePenultimateNote(length, cantusFirmus)) {
      noMotivesApplied.filter(note => counterpointService.applyAntePenultimateCannotBeLeadingToneRule(inMajorKeyNotes, tonic, note))
    } else {
      noMotivesApplied
    }
  }

  // perhaps give it the reason whenever it fails. like a list of pairs [invalidLine to reasonItFailed]
  // then if the reason is that the climax was at the beginning, only pick notes that are higher than
  // the note picked before in that position.

  private def applyFinalNoteAsTonicRule(inMajorKeyNotes: Seq[String], cantusFirmus: List[String], tonic: String, note: String) =
    (note.filterNot(c => c.isDigit) == tonic.filterNot(c => c.isDigit)) && (math.abs(inMajorKeyNotes.indexOf(cantusFirmus.last) - inMajorKeyNotes.indexOf(note)) == 1)

  private def isALargeLeap(note: String, prevNote: String) =
    math.abs(AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note) - AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(prevNote)) >= 5

  private def applyUniversalRules(notes: Seq[String], cantusFirmus: List[String], invalidLines: List[List[String]], length: Int) = {
    val lowestNote = cantusFirmus.map(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)).min
    val highestNote = cantusFirmus.map(note => AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)).max
    val countsOfNotes = cantusFirmus.groupBy(identity).view.mapValues(_.size).toSeq
    notes
      .filter(note => {
        val noteIdx = AVAILABLE_CANTUS_FIRMUS_NOTES.indexOf(note)
        counterpointService.applyNoRepeatedNotesRule(cantusFirmus, note) &&
          counterpointService.applyMaxRangeRule(lowestNote, highestNote, noteIdx) &&
          !invalidLines.contains(cantusFirmus :+ note) &&
          // todo: it can equal the max note if it keeps going higher than it later though
          counterpointService.applySingleClimaxRule(note, cantusFirmus, AVAILABLE_CANTUS_FIRMUS_NOTES) &&
          counterpointService.isMelodicConsonance(cantusFirmus.last, note, AVAILABLE_CANTUS_FIRMUS_NOTES) &&
          counterpointService.applyMaxRepetitionRules(cantusFirmus, countsOfNotes, note, length)
      })
  }
}

private object CantusFirmusService {
  private val MIN_LENGTH = 8
  private val MAX_LENGTH = 16
  private val AVAILABLE_CANTUS_FIRMUS_NOTES: List[String] = GET_ALL_NOTES_BETWEEN_TWO_NOTES("E2", "E4")
  private val MIN_TONIC: Int = 3
  private val MAX_TONIC: Int = AVAILABLE_CANTUS_FIRMUS_NOTES.length - 7
}
