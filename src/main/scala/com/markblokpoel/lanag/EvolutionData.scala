package com.markblokpoel.lanag

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.core.Data

case class EvolutionData(fitness: Vector[(RSA1ShotAgent, BigDecimal)],
                         meanAmbiguity: BigDecimal)
    extends Data
