package com.markblokpoel.lanag.ilasymmetry

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.core.Data

case class EvolutionData(fitness: List[(RSA1ShotAgent, BigDecimal)],
                         meanAmbiguities: List[Double],
                         meanAsymmetries: List[List[Double]],
                         populationTypes: Map[Int, Int]
                        )
    extends Data
