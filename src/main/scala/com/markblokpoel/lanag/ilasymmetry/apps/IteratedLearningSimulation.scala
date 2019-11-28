package com.markblokpoel.lanag.ilasymmetry.apps

import java.io.{File, PrintWriter}

import com.markblokpoel.lanag.ilasymmetry.{Evolution, Generation, Population, TransitionMatrix}
import com.markblokpoel.lanag.util.{ConfigWrapper, RNG}
import com.typesafe.config.ConfigFactory

object IteratedLearningSimulation extends App {

  val conf = ConfigWrapper(ConfigFactory.load())

  val vocabularySize =
    conf.getOrElse[Int]("iterated-learning.vocabulary-size", 2)
  val contextSize = conf.getOrElse[Int]("iterated-learning.context-size", 2)
  val order = conf.getOrElse[Int]("iterated-learning.order", 1)
  val localMode =
    conf.getOrElse[Boolean]("iterated-learning.spark-local-mode", true)

  val transitionMatrixFilename = conf.getOrElse[String](
    "iterated-learning.simulation.matrix-filename",
    "data/default.ser")
  val dataOutputFilename = conf.getOrElse[String](
    "iterated-learning.simulation.data-output-filename",
    "output/default.csv")
  val initialAmbiguity = conf
    .getOrElse[Double]("iterated-learning.simulation.initial-ambiguity", 0.5)
  val populationSize =
    conf.getOrElse[Int]("iterated-learning.simulation.population-size", 10)
  val maxNrGenerations =
    conf.getOrElse[Int]("iterated-learning.simulation.max-nr-generations", 25)
  val beta = conf.getOrElse[Double]("iterated-learning.simulation.beta", 1.0)
  val fitnessNrInteractionRounds = conf.getOrElse[Int](
    "iterated-learning.simulation.fitness-nr-interaction-rounds",
    5)
  val fitnessInterlocutorGroupSize = conf.getOrElse[Int](
    "iterated-learning.simulation.fitness-interlocutor-group-size",
    5)

  val transitionMatrixFilenameBase = conf.getOrElse[String](
    "iterated-learning.compute-transition-matrix.matrix-filename-base",
    "output/default")
  val k =
    conf.getOrElse[Int]("iterated-learning.compute-transition-matrix.k", 5)
  val sampleSize = conf.getOrElse[Int](
    "iterated-learning.compute-transition-matrix.sample-size",
    15)
  val errorRate = conf.getOrElse[Double](
    "iterated-learning.compute-transition-matrix.error-rate",
    0.05)

  //  val ois = new ObjectInputStream(new FileInputStream(transitionMatrixFilename))
  //  val mm = ois.readObject.asInstanceOf[MutationMatrix]
  //  ois.close()

  val transitionMatrix = TransitionMatrix(vocabularySize, contextSize, order, k, sampleSize, errorRate)
  val consistentAgents = transitionMatrix.allPossibleAgents.filter(_.originalLexicon.isConsistent)
  val initialAgents =
    (for(_ <- 0 until populationSize) yield consistentAgents(RNG.nextInt(consistentAgents.length))).toList
  val initialGeneration = Generation(Population(initialAgents), 0)

  val evolution = Evolution(initialGeneration,
    transitionMatrix,
    maxNrGenerations,
    fitnessNrInteractionRounds,
    fitnessInterlocutorGroupSize,
    beta)

  val dataFile = new PrintWriter(new File(dataOutputFilename))
  dataFile.println("generation; agentIdx; fitness; meanAmbiguity; varAmbiguity; meanAsymmetry; varAsymmetry")

  private def stats(seq: Seq[Double]): (Double, Double) = {
    val mean = seq.sum / seq.size
    val variance = 1.0 / seq.size * seq.map(a => math.pow(a - mean, 2)).sum
    (mean, variance)
  }

  val allGenerations = for (gen <- evolution) yield gen
  for ((_, data) <- allGenerations) {
    val generation = data.generation

    for(agentData <- data.agentData) {
      val (meanAsymmetry, varAsymmetry) = stats(agentData.asymmetries)

      dataFile.println(
        s"$generation;" +
        s"${agentData.idx};" +
        s"${agentData.fitness};" +
        s"${agentData.meanAmbiguity};" +
        s"${agentData.varianceAmbiguity};" +
        s"$meanAsymmetry;" +
        s"$varAsymmetry"
      )
    }
    dataFile.flush()

    print(s"$generation / $maxNrGenerations\r")
  }
  dataFile.close()
}
