package com.markblokpoel.lanag.ilasymmetry

case class Evolution(initialGeneration: Generation,
                     transitionMatrix: TransitionMatrix,
                     maxGenerations: Int,
                     interactionRounds: Int,
                     interactionGroupSize: Int,
                     beta: Double = 1.0)
    extends Iterator[(Generation, EvolutionData)] {
  var currentGeneration: Generation = initialGeneration
  assert(beta>0)
  assert(interactionGroupSize>0)

  def hasNext: Boolean = currentGeneration.generation < maxGenerations

  def next(): (Generation, EvolutionData) = {
    val population = currentGeneration.population

    val (data, posteriors) = population.agents
      .map(speaker => {
        val fitness = population.fitnessOf(speaker, interactionRounds, interactionGroupSize)
        val likelihood = transitionMatrix.getLearnerDistribution(speaker)

        val idx = transitionMatrix.lexiconToIdx(speaker.originalLexicon)
        val (meanAmbiguity, varAmbiguity) = speaker.originalLexicon.meanAndVarianceAmbiguity()
        val asymmetries = population.asymmetries(speaker)
        val d = EvolutionAgentData(
          idx,
          fitness,
          meanAmbiguity,
          varAmbiguity,
          asymmetries
        )

        (d, likelihood.prod(BigDecimal(fitness)))
      }).unzip

    val posteriorDistribution =
      posteriors.foldLeft(transitionMatrix.zeroDistribution)(_.add(_))

    val learnerPopulation =
      (for (_ <- 0 until population.size) yield posteriorDistribution.softArgMax(beta))
        .filter(_.isDefined)
        .map(_.get)
        .toList

    val previousData = EvolutionData(currentGeneration.generation, data.toVector)

    currentGeneration = Generation(
      Population(learnerPopulation),
      currentGeneration.generation + 1
    )

    (currentGeneration, previousData)
  }
}
