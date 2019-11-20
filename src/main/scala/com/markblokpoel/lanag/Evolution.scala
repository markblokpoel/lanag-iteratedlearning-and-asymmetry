package com.markblokpoel.lanag

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.math.Distribution

case class Evolution(initialGeneration: Generation,
                     mutationMatrix: MutationMatrix,
                     maxGenerations: Int)
    extends Iterator[(Generation, EvolutionData)] {
  var currentGeneration: Generation = initialGeneration

  def hasNext: Boolean = currentGeneration.generation < maxGenerations

  def next(): (Generation, EvolutionData) = {
    val pop = currentGeneration.population
    val fitness =
      mutationMatrix.allPossibleAgents
        .map(ag => (ag, BigDecimal.double2bigDecimal(pop.fitnessOf(ag, 5))))
        .toVector
    val speakerPriors = Distribution[RSA1ShotAgent](fitness)
    val learnerDistribution = mutationMatrix.posterior(speakerPriors)
    val learnerPopulation =
      (for (_ <- 0 until pop.size) yield learnerDistribution.softArgMax(1.0))
        .filter(_.isDefined)
        .map(_.get)
        .toList
    currentGeneration = Generation(Population(learnerPopulation),
                                   currentGeneration.generation + 1)
    (currentGeneration, EvolutionData(fitness, currentGeneration.meanAmbiguity))
  }
}
