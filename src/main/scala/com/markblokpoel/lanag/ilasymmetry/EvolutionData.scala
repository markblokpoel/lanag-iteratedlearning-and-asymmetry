package com.markblokpoel.lanag.ilasymmetry

import com.markblokpoel.lanag.core.Data

case class EvolutionAgentData(idx: Int,
                              fitness: Double,
                              meanAmbiguity: Double,
                              varianceAmbiguity: Double,
                              asymmetries: List[Double]
                             )

case class EvolutionData(generation: Int,
                         agentData: Vector[EvolutionAgentData]
                        )
    extends Data
