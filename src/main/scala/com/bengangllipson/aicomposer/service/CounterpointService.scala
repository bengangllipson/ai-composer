package com.bengangllipson.aicomposer.service

import com.bengangllipson.aicomposer.service.CounterpointService.*

import scala.util.matching.Regex

private[aicomposer] class CounterpointService {
  def getInterval(bottomNote: String, topNote: String, availableNotes: List[String]): Int =
    availableNotes.indexOf(topNote) - availableNotes.indexOf(bottomNote)

  def isFirstNote(line: List[String]): Boolean = line.isEmpty

  def applyMaxRangeRule(lowestNote: Int, highestNote: Int, noteIdx: Int): Boolean = (noteIdx - lowestNote <= 16 && highestNote - noteIdx <= 16)

  def isLastNote(length: Int, line: List[String]): Boolean = line.length == length - 1

  def isLeadingTone(availableNotes: Seq[String], line: List[String], tonic: String): Boolean =
    line.last.filterNot(c => c.isDigit) == availableNotes(availableNotes.map { note => note.filterNot { c => c.isDigit } }.lastIndexOf(tonic.filterNot(c => c.isDigit)) - 1).filterNot(c => c.isDigit)


  def applyLeadingToneLeadsToTonicRule(line: List[String], note: String, availableNotes: List[String]): Boolean =
    availableNotes.indexOf(note) - availableNotes.indexOf(line.last) == 1

  private def isDownwardsMotion(line: List[String], availableNotes: Seq[String]) =
    availableNotes.indexOf(line(line.length - 2)) - availableNotes.indexOf(line.last) > 0

  private def isALeap(note: String, prevNote: String, inMajorKeyNotes: List[String]): Boolean =
    math.abs(inMajorKeyNotes.indexOf(note) - inMajorKeyNotes.indexOf(prevNote)) > 1

  private def followingALargeLeap(line: List[String], availableNotes: Seq[String]) =
    line.length > 1 &&
      math.abs(availableNotes.indexOf(line.last) - availableNotes.indexOf(line(line.length - 2))) >= 5

  private def followingALeap(line: List[String], inMajorKeyNotes: List[String]) =
    line.length > 1 &&
      isALeap(line.last, line(line.length - 2), inMajorKeyNotes)

  private def followingTwoLeaps(line: List[String], inMajorKeyNotes: List[String]) =
    line.length > 2 &&
      isALeap(line(line.length - 2), line(line.length - 3), inMajorKeyNotes) &&
      isALeap(line.last, line(line.length - 2), inMajorKeyNotes)

  def isAntePenultimateNote(length: Int, line: List[String]): Boolean =
    line.length == length - 3

  def applyAntePenultimateCannotBeLeadingToneRule(inMajorKeyNotes: Seq[String], tonic: String, note: String): Boolean = {
    val lt = inMajorKeyNotes(inMajorKeyNotes.map(note => note.filterNot(c => c.isDigit)).lastIndexOf(tonic.filterNot(c => c.isDigit)) - 1)
    note.filterNot(c => c.isDigit) != lt.filterNot(c => c.isDigit)
  }


  def applyLeapsRules(inMajorKeyNotes: List[String], availableNotes: Seq[String], line: List[String], notes: Seq[String]): Seq[String] =
    if (followingALargeLeap(line, availableNotes)) {
      val next = if (isDownwardsMotion(line, availableNotes)) {
        inMajorKeyNotes(inMajorKeyNotes.indexOf(line.last) + 1)
      } else {
        inMajorKeyNotes(inMajorKeyNotes.indexOf(line.last) - 1)
      }
      if (notes.contains(next)) {
        Seq(next)
      } else {
        Seq()
      }
    } else if (followingALeap(line, inMajorKeyNotes)) {
      if (followingTwoLeaps(line, inMajorKeyNotes)) {
        notes.filter(note => !isALeap(note, line.last, inMajorKeyNotes))
      } else {
        val previousLeapDirection = if (inMajorKeyNotes.indexOf(line(line.length - 2)) - inMajorKeyNotes.indexOf(line.last) > 0) {
          "down"
        } else {
          "up"
        }
        notes.filter(note => {
          val secondLeapDirection = if (inMajorKeyNotes.indexOf(line.last) - inMajorKeyNotes.indexOf(note) > 0) {
            "down"
          } else {
            "up"
          }
          !(isALeap(note, line.last, inMajorKeyNotes)) || previousLeapDirection != secondLeapDirection
        })
      }
    } else {
      notes
    }


  private def isInBackQuarter(line: List[String], length: Int) = {
    line.length >= (length - (length / 4))
  }

  private def isTheNoteBeforeBackQuarter(line: List[String], length: Int) = {
    line.length == (length - (length / 4)) - 1
  }

  private def isInFirstQuarter(line: List[String], length: Int) = {
    line.length <= (length / 4)
  }

  private def isInMiddle(line: List[String], length: Int) = {
    line.length < (length / 4) && line.length < (length - (length / 4))
  }

  private def directionIsUpwards(line: List[String], availableNotes: Seq[String], note: String) = {
    availableNotes.indexOf(note) > availableNotes.indexOf(line.last)
  }

  private def getMaxNote(line: List[String], availableNotes: Seq[String]) = {
    line.map(note => availableNotes.indexOf(note)).max
  }

  private def noteIsInFirstThird(note: String, line: List[String], length: Int) = {
    line.indexOf(note) < (length / 3)
  }

  private def climaxIsInTheBeginning(line: List[String], availableNotes: Seq[String], length: Int) = {
    line.filter(note => noteIsInFirstThird(note, line, length)).map(note => availableNotes.indexOf(note)).contains(getMaxNote(line, availableNotes))
  }

  private def noteIsLowerThanClimax(line: List[String], availableNotes: Seq[String], note: String) = {
    availableNotes.indexOf(note) < getMaxNote(line, availableNotes)
  }

  def applyClimaxMustBeInMiddleRule(line: List[String], availableNotes: Seq[String], length: Int, notes: Seq[String]): Seq[String] = {
    if (isInBackQuarter(line, length)) {
      notes.filter(note => noteIsLowerThanClimax(line, availableNotes, note))
    } else if (isInFirstQuarter(line, length)) {
      notes.filter(note => availableNotes.indexOf(note) < 19)
    } else if (isTheNoteBeforeBackQuarter(line, length) && climaxIsInTheBeginning(line, availableNotes, length)) {
      notes.filter(note => availableNotes.indexOf(note) > getMaxNote(line, availableNotes))
    } else if (isInMiddle(line, length) && climaxIsInTheBeginning(line, availableNotes, length)) {
      notes.filter(note => directionIsUpwards(line, availableNotes, note))
    } else {
      notes
    }
  }

  def applyNoRepeatedNotesRule(line: List[String], note: String): Boolean = {
    note != line.last
  }

  private def applyMaxRepetitionRule(countsOfNotes: Seq[(String, Int)], note: String) = {
    countsOfNotes.filter(can => can._1 == note).head._2 < 2
  }

  private def applyTonicMaxRepetitionRule(countsOfNotes: Seq[(String, Int)], note: String, isInFirstHalf: Boolean) = {
    if (isInFirstHalf) {
      false
    } else {
      countsOfNotes.filter(can => can._1 == note).head._2 < 4
    }
  }

  private def noteIsTonic(line: List[String], note: String) = {
    note == line.head
  }

  private def noteHasOccurredPreviously(countsOfNotes: Seq[(String, Int)], note: String) = {
    countsOfNotes.map(nac => nac._1).contains(note)
  }

  def applyMaxRepetitionRules(line: List[String], countsOfNotes: Seq[(String, Int)], note: String, length: Int): Boolean = {
    if (noteIsTonic(line, note) && noteHasOccurredPreviously(countsOfNotes, note)) {
      applyTonicMaxRepetitionRule(countsOfNotes, note, line.length < length / 2)
    } else if (noteHasOccurredPreviously(countsOfNotes, note)) {
      applyMaxRepetitionRule(countsOfNotes, note)
    } else {
      true
    }
  }

  def applyFsMaxRepetitionRule(countsOfNotes: Seq[(String, Int)], note: String): Boolean = {
    if (noteHasOccurredPreviously(countsOfNotes, note)) {
      countsOfNotes.filter(can => can._1 == note).head._2 < 4
    } else {
      true
    }
  }

  def isPenultimateNote(length: Int, line: List[String]): Boolean =
    line.length == length - 2


  def applyPenultimateStepFromAboveRule(inMajorKeyNotes: Seq[String], tonic: String): Seq[String] = {
    inMajorKeyNotes.filter(note => {
      val noteWithoutOctave = note.filterNot(c => c.isDigit)
      noteWithoutOctave == inMajorKeyNotes(inMajorKeyNotes.indexOf(tonic) + 1).filterNot(c => c.isDigit)
    })
  }

  def applyPenultimateStepFromBelowRule(inMajorKeyNotes: Seq[String], tonic: String): Seq[String] = {
    inMajorKeyNotes.filter(note => {
      val noteWithoutOctave = note.filterNot(c => c.isDigit)
      val lt = inMajorKeyNotes(inMajorKeyNotes.map(note => note.filterNot(c => c.isDigit)).lastIndexOf(tonic.filterNot(c => c.isDigit)) - 1)
      noteWithoutOctave == lt
    })
  }

  def applyPenultimateStepwiseMotionRule(inMajorKeyNotes: Seq[String], tonic: String, cantusFirmusNote: String = ""): Seq[String] = {
    inMajorKeyNotes.filter(note => {
      // todo: if the fourth to last note is the LT use the 2
      val noteWithoutOctave = note.filterNot(c => c.isDigit)
      val lt = inMajorKeyNotes(inMajorKeyNotes.map(note => note.filterNot(c => c.isDigit)).lastIndexOf(tonic.filterNot(c => c.isDigit)) - 1)
      if (cantusFirmusNote.isEmpty) {
        noteWithoutOctave == lt.filterNot(c => c.isDigit) ||
          noteWithoutOctave == inMajorKeyNotes(inMajorKeyNotes.indexOf(tonic) + 1).filterNot(c => c.isDigit)
      } else {
        val cfNoteWithoutOctave = cantusFirmusNote.filterNot(c => c.isDigit)
        if (cfNoteWithoutOctave == lt.filterNot(c => c.isDigit)) {
          noteWithoutOctave == inMajorKeyNotes(inMajorKeyNotes.indexOf(tonic) + 1).filterNot(c => c.isDigit)
        } else {
          noteWithoutOctave == lt.filterNot(c => c.isDigit)
        }
      }
    })
  }

  def formatOutput(line: List[String]): List[String] = {
    line.map(note => {
      val tonic = line.head.dropRight(1)
      if (isSecondaryNoteAndTonicOfSharpKey(note, tonic)) {
        formatSharpKeySecondaryNote(note)
      } else if (isSecondaryNoteAndTonicOfFlatKey(note, tonic)) {
        formatFlatKeySecondaryNote(note)
      } else {
        formatPrimaryNote(note)
      }
    })
  }

  def applySingleClimaxRule(note: String, line: List[String], availableNotes: List[String]): Boolean = {
    note != line.maxBy(cantusFirmusNote => availableNotes.indexOf(cantusFirmusNote))
  }

  def formatInput(line: List[String]): List[String] = {
    line.map(note => {
      note.split("/").map(subNote => {
        val upperNoteName = subNote.charAt(0).toUpper.toString
        val sharpOrFlatSymbol = subNote.takeRight(subNote.length - 1)
        if (sharpOrFlatSymbol.nonEmpty) {
          getCorrespondingFullNote(upperNoteName.concat(sharpOrFlatSymbol))
        } else {
          upperNoteName.concat(sharpOrFlatSymbol)
        }
      }).mkString("")
    })
  }

  def getInMajorKeyNotes(tonic: String, availableNotes: List[String]): List[String] =
    availableNotes.filter(note => {
      noteIsInMajorKey(tonic, note, availableNotes.indices
        .filter(interval => intervalIsInMajorKey(interval)), availableNotes)
    })

  def isMelodicConsonance(lastNote: String, note: String, availableNotes: List[String]): Boolean = {
    isConsonance(lastNote, note, availableNotes, MELODIC_CONSONANCES)
  }

  def isHarmonicConsonance(firstNote: String, secondNote: String, availableNotes: List[String]): Boolean = {
    isConsonance(firstNote, secondNote, availableNotes, HARMONIC_CONSONANCES)
  }


  private def isConsonance(firstNote: String, secondNote: String, availableNotes: List[String], consonances: Set[Int]) = {
    val firstNoteIdx = availableNotes.indexOf(firstNote)
    val secondNoteIdx = availableNotes.indexOf(secondNote)
    firstNoteIdx != -1 && secondNoteIdx != -1 &&
      consonances
        .contains(math.abs(availableNotes.indexOf(firstNote) - availableNotes.indexOf(secondNote)))
  }

  private def noteIsInMajorKey(tonic: String, note: String, majorKeyIntervals: Seq[Int], availableNotes: List[String]) =
    majorKeyIntervals
      .map(interval => {
        getNoteForInterval(tonic, interval, availableNotes)
      })
      .contains(note)

  private def getNoteForInterval(tonic: String, interval: Int, availableNotes: List[String]) = {
    val newTonic = if (availableNotes.indexOf(tonic) < 0) {
      val note = tonic.substring(0, tonic.length - 1)
      val octave = (tonic.substring(tonic.length - 1, tonic.length).toInt + 1).toString
      note + octave
    } else {
      tonic
    }
    availableNotes(
      (availableNotes.indexOf(newTonic) + interval) %
        (OCTAVE * 2)
    )
  }

  private def intervalIsInMajorKey(interval: Int) =
    MAJOR_KEY_INTERVALS.contains(interval % OCTAVE)

  private def isSecondaryNoteAndTonicOfSharpKey(note: String, key: String): Boolean = {
    isSecondaryNote(note) && isTonicOfSharpKey(key)
  }

  private def isTonicOfSharpKey(key: String) = {
    SHARP_KEYS.contains(key)
  }

  private def isSecondaryNote(note: String) = {
    note.contains("#")
  }

  private def formatSharpKeySecondaryNote(note: String): String = {
    note.split("/").head.toLowerCase + "/" + note.last
  }

  private def isSecondaryNoteAndTonicOfFlatKey(note: String, tonic: String): Boolean = {
    isSecondaryNote(note) && isTonicOfFlatKey(tonic)
  }

  private def isTonicOfFlatKey(tonic: String) = {
    if (isSecondaryNote(tonic)) {
      FLAT_KEYS.contains(tonic.split("/")(1))
    } else {
      FLAT_KEYS.contains(tonic)
    }
  }

  private def formatFlatKeySecondaryNote(note: String): String = {
    note.split("/")(1).toLowerCase.dropRight(1) + "/" + note.last
  }

  private def formatPrimaryNote(note: String): String = {
    note.toLowerCase.dropRight(1) + "/" + note.last
  }

  private def getCorrespondingFullNote(note: String): String = {
    NOTES.filter(fullNote => fullNote.contains(note)).head
  }
}

object CounterpointService {
  private val SHARP_KEYS: Set[String] = Set("A", "B", "C", "D", "E", "G")
  private val FLAT_KEYS: Set[String] = Set("F", "Bb", "Eb", "Ab", "Db", "Gb")
  private val NOTES: List[String] = List(
    "E",
    "F",
    "F#/Gb",
    "G",
    "G#/Ab",
    "A",
    "A#/Bb",
    "B",
    "C",
    "C#/Db",
    "D",
    "D#/Eb"
  )
  private val MAJOR_KEY_INTERVALS: Set[Int] = Set(
    0, 2, 4, 5, 7, 9, 11
  )
  private val OCTAVE = 12

  val MELODIC_CONSONANCES: Set[Int] = Set(
    1, 2, 3, 4, 5, 7, 8, 9, 12
  )

  private val HARMONIC_CONSONANCES: Set[Int] = Set(
    0, 3, 4, 5, 7, 8, 9, 12
  )

  val PERFECT_INTERVALS: Set[Int] = Set(
    0, 5, 7, 12, 17, 19
  )

  private val NOTE_MATCHER: Regex = """(.+)(\d+)""".r

  private def SEPARATE_NOTE_AND_OCTAVE(note: String): (String, Int) =
    note match {
      case NOTE_MATCHER(note, octave) => (note, octave.toInt)
    }

  def GET_ALL_NOTES_BETWEEN_TWO_NOTES(lowNote: String, highNote: String): List[String] = {
    val (lowNoteName, lowOctave) =
      SEPARATE_NOTE_AND_OCTAVE(
        lowNote
      )
    val (highNoteName, highOctave) =
      SEPARATE_NOTE_AND_OCTAVE(
        highNote
      )

    val firstNoteIsAtOrAboveC = NOTES.indexOf(lowNoteName) >= NOTES.indexOf("C")
    val (_, highOctaveWithCSplit) = if (firstNoteIsAtOrAboveC) {
      val (noteName, octave) = (highNote, highOctave)
      (noteName, octave + 1)
    } else {
      (highNote, highOctave)
    }
    (lowOctave to highOctaveWithCSplit).flatMap(octave => {
      val filteredNotes = if (octave == lowOctave) {
        NOTES.filter(note => NOTES.indexOf(note) >= NOTES.indexOf(lowNoteName))
      } else if (octave == highOctaveWithCSplit) {
        NOTES.filter(note => NOTES.indexOf(note) <= NOTES.indexOf(highNoteName))
      } else {
        NOTES
      }

      filteredNotes.map(note => {
        if (NOTES.indexOf(note) < NOTES.indexOf("C") && firstNoteIsAtOrAboveC) {
          note.concat((octave - 1).toString)
        } else if (NOTES.indexOf(note) < NOTES.indexOf("C") && !firstNoteIsAtOrAboveC) {
          note.concat(octave.toString)
        } else if (NOTES.indexOf(note) >= NOTES.indexOf("C") && firstNoteIsAtOrAboveC) {
          note.concat(octave.toString)
        } else if (NOTES.indexOf(note) >= NOTES.indexOf("C") && !firstNoteIsAtOrAboveC) {
          note.concat((octave + 1).toString)
        } else {
          // will never hit
          note.concat(octave.toString)
        }
      })
    }).toList
  }
}
