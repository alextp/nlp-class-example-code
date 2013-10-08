package edu.umass.cs.iesl.apassos

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.load._
import cc.factorie.app.nlp.mention.{MentionType, Mention, MentionList}
import cc.factorie.app.nlp.coref.{EntityKey, ConllCorefLoader}
import cc.factorie.app.nlp.hcoref.{EntityVariable, EntityRef}

/**
 * User: apassos
 * Date: 10/7/13
 * Time: 2:20 PM
 */



object Lecture5AceLoader {
  def printMentions(aceDoc: Document): Unit = {
    val mentionList = aceDoc.attr[MentionList]
    val entityMap = collection.mutable.HashMap[String,Int]()
    val startToMentionMap = mentionList.map(m => (m.section, m.start) -> m).groupBy(_._1).toMap
    val endToMentionMap = mentionList.map(m => (m.section,m.end-1) -> m).groupBy(_._1).toMap
    var count = 0
    for (t <- aceDoc.tokens) {
      var s = ""
      if (startToMentionMap.contains((t.section,t.positionInSection))) {
        for ((_,m) <- startToMentionMap((t.section,t.positionInSection))) s += "["+m.attr[MentionType].categoryValue+" "
      }
      s += t.string
      if (endToMentionMap.contains((t.section,t.positionInSection))) {
        for ((_,m) <- endToMentionMap((t.section,t.positionInSection))) {
          s += "]"
          val id = entityMap.getOrElseUpdate(m.attr[EntityRef].entity.string, entityMap.size)
          s += s"($id)"
        }
      }
      if (t.hasNext && !t.next.string.matches("\\.|,|;|\\?")) s += " "
      count += s.length
      print(s)
      if (count >= 70) { count = 0; println() }
    }
  }

  def main(args: Array[String]): Unit = {
    val aceDoc = LoadACE.fromApf("/iesl/data/ldc/LDC2006T06/data/english/bc/fp1/cnn_cf_20030303.1900.00.apf.xml")
    val aceMentions = aceDoc.attr[ACEMentionSpanList]
    val mentionList = aceDoc.attr += new MentionList
    aceMentions.foreach(a => {
      val m = new Mention(a.section, a.start, a.length, a.length-1)
      mentionList += m
      m.attr += new MentionType(m, a.attr[ACEMentionIdentifiers].mType)
      m.attr += a.attr[EntityRef]
    })
    printMentions(aceDoc)
  }
}

object Lecture5OntonotesLoader {
  def processEntity(name: String): String = {
    val s = name.split("-")
    if (s.length == 2) s.last else ""
  }

  def printMentions(aceDoc: Document): Unit = {
    val mentionList = aceDoc.attr[MentionList]
    val startToMentionMap = mentionList.map(m => (m.section, m.start) -> m).groupBy(_._1).toMap
    val endToMentionMap = mentionList.map(m => (m.section,m.end-1) -> m).groupBy(_._1).toMap
    var count = 0
    for (t <- aceDoc.tokens) {
      var s = ""
      if (startToMentionMap.contains((t.section,t.positionInSection))) {
        for ((_,m) <- startToMentionMap((t.section,t.positionInSection))) s += "["
      }
      s += t.string
      if (endToMentionMap.contains((t.section,t.positionInSection))) {
        for ((_,m) <- endToMentionMap((t.section,t.positionInSection))) s += "]"+processEntity(m.attr[EntityKey].name)
      }
      if (t.hasNext && !t.next.string.matches("\\.|,|;|\\?")) s += " "
      count += s.length
      print(s)
      if (count >= 70) { count = 0; println() }
    }
  }

  def main(args: Array[String]): Unit = {
    val ontonotesDoc = "/iesl/canvas/mccallum/data/conll2011/conll-train-clean.txt"
    val docs = ConllCorefLoader.loadWithParse(ontonotesDoc)
    printMentions(docs.head)
  }
}
