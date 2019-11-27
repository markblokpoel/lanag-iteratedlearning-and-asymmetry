package com.markblokpoel.lanag.ilasymmetry.apps

import java.io.{File, FileInputStream, ObjectInputStream, PrintWriter}

import com.markblokpoel.lanag.ilasymmetry.{Evolution, Generation, MutationMatrix, Population}
import com.markblokpoel.lanag.util.ConfigWrapper
import com.typesafe.config.ConfigFactory

import scala.util.Random

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
  val fitnessInterlocutorGroupSize = conf.getOrElse[Double](
    "iterated-learning.simulation.fitness-interlocutor-group-size",
    5)

  val ois = new ObjectInputStream(new FileInputStream(transitionMatrixFilename))
  val mm = ois.readObject.asInstanceOf[MutationMatrix]
  ois.close()

  val consistentAgents = mm.allPossibleAgents.filter(_.originalLexicon.isConsistent)
  val initialAgents = Random.shuffle(consistentAgents).take(populationSize)
  val initialGeneration = Generation(Population(initialAgents), 0)

  val evolution = Evolution(initialGeneration,
                            mm,
                            maxNrGenerations,
                            fitnessNrInteractionRounds,
                            fitnessInterlocutorGroupSize,
                            beta)

  val dataFile = new PrintWriter(new File(dataOutputFilename))
  dataFile.println(
    "generation; meanFitness; meanAmbiguity; varAmbiguity; meanAsymmetry; varAsymmetry")

  val dataFile2 = new PrintWriter(new File("output/populationTypes.csv"))
  dataFile2.println("generation; agentId; count")

  private def stats(seq: Seq[Double]): (Double, Double) = {
    val mean = seq.sum / seq.size
    val variance = 1.0 / seq.size * seq.map(a => math.pow(a - mean, 2)).sum
    (mean, variance)
  }

  private def stats2(seq: Seq[Seq[Double]]): (Double, Double) = {
    val subStats = for (values <- seq) yield stats(values)
    val (means, _) = subStats.unzip
    stats(means)
  }

  val allGenerations = for (gen <- evolution) yield gen
  for ((gen, data) <- allGenerations) {
    val meanFitness = data.fitness.foldLeft(0.0)((acc, f) =>
      f._2.doubleValue() + acc) / gen.population.size
    val (meanAmbiguity, varAmbiguity) = stats(data.meanAmbiguities)
    val (meanAsymmetry, varAsymmetry) = stats2(data.meanAsymmetries)

    dataFile.println(
      s"${gen.generation}; $meanFitness; $meanAmbiguity; $varAmbiguity; $meanAsymmetry; $varAsymmetry")


    // Write population distribution
    for(key <- data.populationTypes.keySet) {
      val count = data.populationTypes.get(key)
      dataFile2.println(s"${gen.generation}; $key; $count")
    }
    print(f"${gen.generation}%3d / $maxNrGenerations%3d\r")
    dataFile.flush()
    dataFile2.flush()
  }
  dataFile.close()
  dataFile2.close()



}
