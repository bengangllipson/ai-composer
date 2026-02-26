package com.bengangllipson.aicomposer

import com.bengangllipson.aicomposer.CounterpointService.{FLAT_KEYS, NOTES, SHARP_KEYS}

private class CounterpointService {
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

private object CounterpointService {
  val SHARP_KEYS: Set[String] = Set("A", "B", "C", "D", "E", "G")
  val FLAT_KEYS: Set[String] = Set("F", "Bb", "Eb", "Ab", "Db", "Gb")
  val NOTES: List[String] = List(
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
}
