package com.markblokpoel.lanag.ilasymmetry

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.math.Distribution

case class Evolution(initialGeneration: Generation,
                     mutationMatrix: MutationMatrix,
                     maxGenerations: Int,
                     interactionRounds: Int,
                     interactionPercentage: Double,
                     beta: Double = 1.0)
    extends Iterator[(Generation, EvolutionData)] {
  var currentGeneration: Generation = initialGeneration
  assert(beta>0)
  assert(interactionPercentage>0.0)
  assert(interactionPercentage<=1.0)

  def hasNext: Boolean = currentGeneration.generation < maxGenerations

  def next(): (Generation, EvolutionData) = {
    val pop = currentGeneration.population
    val fitness =
      mutationMatrix.allPossibleAgents
        .map(ag => (ag, BigDecimal.double2bigDecimal(pop.fitnessOf(ag, interactionRounds, interactionPercentage))))
    val speakerPriors = Distribution[RSA1ShotAgent](fitness.toVector)
    val learnerDistribution = mutationMatrix.posterior(speakerPriors)
    val learnerPopulation =
      (for (_ <- 0 until pop.size) yield learnerDistribution.softArgMax(beta))
        .filter(_.isDefined)
        .map(_.get)
        .toList
    currentGeneration = Generation(
      Population(learnerPopulation),
      currentGeneration.generation + 1
    )

    val populationTypes = learnerPopulation.map(ag => mutationMatrix.allPossibleAgents.indexOf(ag)).groupBy(identity).mapValues(_.size)
    val currentData = EvolutionData(
      fitness,
      currentGeneration.ambiguities,
      currentGeneration.asymmetries,
      populationTypes
    )
    (currentGeneration, currentData)
  }
}
