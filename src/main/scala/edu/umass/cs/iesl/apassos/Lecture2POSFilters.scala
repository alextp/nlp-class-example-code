package edu.umass.cs.iesl.apassos

import cc.factorie.app.nlp.{TokenSpan, Token}

import cc.factorie._
import cc.factorie.app.nlp.load.LoadOntonotes5

/**
 * User: apassos
 * Date: 9/23/13
 * Time: 11:12 AM
 */

object Lecture2POSFilters {

  def tokensToPhrase(ts: Iterable[Token]): String = new TokenSpan(ts.head.section, ts.head.positionInSection, ts.size).phrase.replace("\n", " ")

  def main(args: Array[String]) {
    implicit val rng = new scala.util.Random(0)
    val trainDoc = LoadOntonotes5.fromFilename("/iesl/canvas/mccallum/data/ontonotes-en-1.1.0/trn-pmd/nw-wsj-trn.dep.pmd").head
    val tokens = trainDoc.tokens.toSeq.shuffle
    for (tag <- app.nlp.pos.PennPosDomain.categories) {
      println(s"Tag: $tag")
      for (tok <- tokens.filter(_.posLabel.categoryValue == tag).take(10)) {
        println(f"${tokensToPhrase(tok.prevWindow(4))}%40s (${tok.string}%12s) ${tokensToPhrase(tok.nextWindow(4))}")
      }
      println()
    }
  }
}

object Lecture2POSFiltersPOS {

  def tokensToPhrase(ts: Iterable[Token]): String = ts.map(_.posLabel.categoryValue).mkString(" ")

  def main(args: Array[String]) {
    implicit val rng = new scala.util.Random(0)
    val trainDoc = LoadOntonotes5.fromFilename("/iesl/canvas/mccallum/data/ontonotes-en-1.1.0/trn-pmd/nw-wsj-trn.dep.pmd").head
    val tokens = trainDoc.tokens.toSeq.shuffle
    for (tag <- app.nlp.pos.PennPosDomain.categories) {
      println(s"Tag: $tag")
      for (tok <- tokens.filter(_.posLabel.categoryValue == tag).take(10)) {
        println(f"${tokensToPhrase(tok.prevWindow(4))}%40s (${tok.string}%12s) ${tokensToPhrase(tok.nextWindow(4))}")
      }
      println()
    }
  }
}

object Lecture2POSFiltersGrep {

  def tokensToPhrase(ts: Iterable[Token]): String = ts.map(t => s"${t.string}/${t.posLabel.categoryValue}").mkString(" ")

  def main(args: Array[String]) {
    implicit val rng = new scala.util.Random(0)
    val trainDoc = LoadOntonotes5.fromFilename("/iesl/canvas/mccallum/data/ontonotes-en-1.1.0/trn-pmd/nw-wsj-trn.dep.pmd").head
    val tokens = trainDoc.tokens.toSeq.shuffle
    println("Type the word")
    val example = readLine()
    for (tok <- tokens.filter(_.string.toLowerCase == example)) {
      println(f"${tokensToPhrase(tok.prevWindow(4))}%60s (${tok.string}%12s)/${tok.posLabel.categoryValue} ${tokensToPhrase(tok.nextWindow(4))}")
    }
  }
}


object Lecture2POSFiltersContext {

  def tokensToPhrase(ts: Iterable[Token]): String = ts.map(t => s"${t.string}/${t.posLabel.categoryValue}").mkString(" ")

  def getContext(t: Token): (String,String) = if (t.hasPrev && t.hasNext) (t.prev.posLabel.categoryValue,t.next.posLabel.categoryValue) else ("","")

  def main(args: Array[String]) {
    implicit val rng = new scala.util.Random(0)
    val trainDoc = LoadOntonotes5.fromFilename("/iesl/canvas/mccallum/data/ontonotes-en-1.1.0/trn-pmd/nw-wsj-trn.dep.pmd").head
    val tokens = trainDoc.tokens.toSeq.shuffle
    for (t <- tokens.take(20)) {
      println(s"Token ${t.string} with tag ${t.posLabel.categoryValue}. Context: ${t.prev.posLabel.categoryValue} _ ${t.next.posLabel.categoryValue}")
      for (tok <- tokens.shuffle.filter(tt => getContext(tt) == getContext(t)).take(20)) {
        println(f"${tokensToPhrase(tok.prevWindow(4))}%60s (${tok.string}%12s)/${tok.posLabel.categoryValue} ${tokensToPhrase(tok.nextWindow(4))}")
      }
      println()
    }
  }
}
