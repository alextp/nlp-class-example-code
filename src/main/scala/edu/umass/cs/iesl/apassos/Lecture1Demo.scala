package edu.umass.cs.iesl.apassos

import cc.factorie._

object Lecture1Demo {
  def main(args: Array[String]) {
    val doc = new app.nlp.Document("All work and no play makes Jack a dull boy. All work and no play makes Jack a dull boy.")
    val pipeline = app.nlp.DocumentAnnotatorPipeline[app.nlp.ner.NerLabel,app.nlp.parse.ParseTree]
    pipeline.process(doc)
    val printers = for (ann <- Seq(app.nlp.pos.POS1, app.nlp.ner.BasicConllNER, app.nlp.parse.TransitionParser)) yield (t: app.nlp.Token) => ann.tokenAnnotationString(t)
    println(doc.owplString(printers))
  }
}