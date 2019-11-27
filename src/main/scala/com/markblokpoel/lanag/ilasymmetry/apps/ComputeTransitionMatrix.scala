package com.markblokpoel.lanag.ilasymmetry.apps

import java.io.{FileOutputStream, ObjectOutputStream}

import com.markblokpoel.lanag.ilasymmetry.MutationMatrix
import com.markblokpoel.lanag.util.{ConfigWrapper, SparkSimulation}
import com.typesafe.config.ConfigFactory

object ComputeTransitionMatrix extends App {
  val conf = ConfigWrapper(ConfigFactory.load())

  val vocabularySize =
    conf.getOrElse[Int]("iterated-learning.vocabulary-size", 2)
  val contextSize = conf.getOrElse[Int]("iterated-learning.context-size", 2)
  val order = conf.getOrElse[Int]("iterated-learning.order", 1)
  val localMode =
    conf.getOrElse[Boolean]("iterated-learning.spark-local-mode", true)

  val transitionMatrixFilenameBase = conf.getOrElse[String](
    "iterated-learning.compute-transition-matrix.matrix-filename-base",
    "output/default")
  val k =
    conf.getOrElse[Int]("iterated-learning.compute-transition-matrix.k", 5)
  val sampleSize = conf.getOrElse[Int](
    "iterated-learning.compute-transition-matrix.sample-size",
    15)

  val sm = SparkSimulation(localMode, cores = 0)

  val mm: MutationMatrix = MutationMatrix(
    vocabularySize,
    contextSize,
    order,
    sparkContext = sm.context,
    k,
    sampleSize
  )
  sm.shutdown()

  val oos = new ObjectOutputStream(
    new FileOutputStream(transitionMatrixFilenameBase + ".ser"))
  oos.writeObject(mm)
  oos.close()
//
//  val ois = new ObjectInputStream(
//    new FileInputStream(transitionMatrixFilenameBase + ".ser"))
//  val mmRead = ois.readObject.asInstanceOf[MutationMatrix]
//  ois.close()
//
//  val pw = new PrintWriter(new File(transitionMatrixFilenameBase + ".csv"))
//  for (ag1 <- mmRead.allPossibleAgents)
//    pw.println(
//      (for (ag2 <- mmRead.allPossibleAgents)
//        yield mmRead(ag1, ag2)).mkString(";"))
//  pw.close()

}
