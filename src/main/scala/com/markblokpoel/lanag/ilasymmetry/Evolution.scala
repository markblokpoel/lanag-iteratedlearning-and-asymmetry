package com.markblokpoel.lanag.ilasymmetry

case class Evolution(initialGeneration: Generation,
                     transitionMatrix: TransitionMatrix,
                     maxGenerations: Int,
                     interactionRounds: Int,
                     interactionGroupSize: Int,
                     beta: Double)
    extends Iterator[(Generation, EvolutionData)] {
  var currentGeneration: Generation = initialGeneration
  require(
    interactionGroupSize > 0,
    s"Variable interactionGroupSize must be >0 but is $interactionGroupSize.")

  def hasNext: Boolean = currentGeneration.generation <= maxGenerations

  def next(): (Generation, EvolutionData) = {
    var progressCount = 0
    val (data, posteriors) = currentGeneration.population.agents
      .map(speaker => {
        val idx = transitionMatrix.lexiconToIdx(speaker.originalLexicon)
        val fitness = currentGeneration.population
          .fitnessOf(speaker, interactionRounds, interactionGroupSize)
        val (meanAmbiguity, varAmbiguity) =
          speaker.originalLexicon.meanAndVarianceAmbiguity()
        val asymmetries = currentGeneration.population.asymmetries(speaker)
        val dat = EvolutionAgentData(idx,
                                     fitness,
                                     meanAmbiguity,
                                     varAmbiguity,
                                     asymmetries)

        val likelihood = transitionMatrix.getLearnerDistribution(speaker)
        val posterior = likelihood prodNotNorm BigDecimal(fitness)

        print(
          progressBar(currentGeneration.generation,
                      progressCount,
                      currentGeneration.population.size) + "\r")
        progressCount += 1

        (dat, posterior)
      })
      .unzip

    val posteriorDistribution =
      posteriors.foldLeft(transitionMatrix.zeroDistribution)(_ addNotNorm _)

    val learnerPopulation =
      posteriorDistribution.sample(currentGeneration.population.size)
//    val learnerPopulation =
//      (for(_ <- 0 until currentGeneration.population.size) yield posteriorDistribution.softArgMax(beta).get)
//        .toList

    val previousData =
      EvolutionData(currentGeneration.generation, data.toVector)

    currentGeneration = Generation(Population(learnerPopulation),
                                   currentGeneration.generation + 1)

    (currentGeneration, previousData)
  }

  private def progressBar(g: Int, i: Int, total: Int): String = {
    val _i = (i.toDouble / total * 20).toInt

    s"$g/$maxGenerations\t[" +
      (0 until _i).map(_ => "#").mkString("") +
      (_i until 20).map(_ => " ").mkString("") +
      "]"
  }
}
