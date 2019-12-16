package com.markblokpoel.lanag.ilasymmetry.parallel

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.ilasymmetry.{EvolutionData, TransitionMatrix}
import com.markblokpoel.lanag.util.SparkSimulation

case class ParallelEvolution(initialGeneration: List[RSA1ShotAgent],
                             transitionMatrix: TransitionMatrix,
                             maxGenerations: Int,
                             interactionRounds: Int,
                             interactionGroupSize: Int,
                             localMode: Boolean,
                             beta: Double = 1.0) {
  assert(beta > 0)
  assert(interactionGroupSize > 0)

  val spark = SparkSimulation(localMode, 0)

  def run(): List[EvolutionData] = {
    ???
  }

}
