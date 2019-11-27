package com.markblokpoel.lanag.ilasymmetry

case class Generation(population: Population, generation: Int) {
  def ambiguities: List[Double] =
    for (ag <- population.agents) yield ag.originalLexicon.meanAmbiguity()

  def asymmetries: List[List[Double]] =
    for (ag1 <- population.agents)
      yield
        for (ag2 <- population.agents)
          yield ag1.originalLexicon.asymmetryWith(ag2.originalLexicon)
}
