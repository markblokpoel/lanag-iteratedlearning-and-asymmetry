package com.markblokpoel.lanag.ilasymmetry.apps

//import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.ilasymmetry.TransitionMatrix
//import com.markblokpoel.lanag.rsa.Lexicon
import com.markblokpoel.lanag.util.SparkSimulation

object MM2CSV extends App {

  val sp = SparkSimulation(local = true, 0)
//
  val t = TransitionMatrix(3, 3, 1, 10, 50, 0.0, sp)

  val d = t.getLearnerDistribution(t.allPossibleAgents(112))
  println(d.distribution)
//
//  val lexicon = Lexicon(
//    Vector(
//      Vector(1.0, 0.0, 0.0),
//      Vector(0.0, 1.0, 0.0),
//      Vector(0.0, 0.0, 1.0)
//    ))
//
//  val distribution = t.getLearnerDistribution(new RSA1ShotAgent(lexicon, 1))
//  val id = t.lexiconToIdx(lexicon)
//
//  println((distribution.domain zip distribution.distribution).mkString("\n"))
//  println(s"speaker id is $id")
////
////  val stuff = for(ag <- t.allPossibleAgents) yield {
////    " " + ag.originalLexicon.toString() +
////    (t.getLearnerDistribution(ag).domain zip t.getLearnerDistribution(ag).distribution).mkString("\n").toString
////  }
//
//  sp.shutdown()

//  println(stuff.mkString("\n"))

  println(t.idxToLexicon(3))

}
