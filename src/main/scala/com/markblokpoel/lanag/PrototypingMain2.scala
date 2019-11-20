package com.markblokpoel.lanag

import java.io.{FileInputStream, ObjectInputStream}

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.rsa.Lexicon

object PrototypingMain2 extends App {
  val vocabularySize = 2
  val contextSize = 2
  val order = 1

  val ois = new ObjectInputStream(
    new FileInputStream(s"output/q-v$vocabularySize-c$contextSize-o$order.obj"))
  val mm = ois.readObject.asInstanceOf[MutationMatrix]
  ois.close()

  val initialAgents =
    (for (_ <- 0 until 10)
      yield
        Lexicon.generateConsistentAmbiguityMapping(contextSize / 2,
                                                   vocabularySize,
                                                   contextSize))
      .map(l => new RSA1ShotAgent(l, 1))
      .toList
  val initialGeneration = Generation(Population(initialAgents), 0)

  val evolution = Evolution(initialGeneration, mm, 10)
  val allGenerations = for (gen <- evolution) yield gen
  for ((gen, data) <- allGenerations) {
    val meanFitness = data.fitness.foldLeft(0.0)((acc, f) =>
      f._2.doubleValue() + acc) / gen.population.size
    println(
      f"${gen.generation}%4d  /${evolution.maxGenerations}%4d -> " +
        f"fitness: $meanFitness%.3f ambiguity: ${data.meanAmbiguity}%.4f")
  }
}
