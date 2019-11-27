package com.markblokpoel.lanag.ilasymmetry

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.core.{ContentSignal, ReferentialIntention}
import com.markblokpoel.lanag.math.Distribution
import com.markblokpoel.lanag.rsa.Lexicon
import com.markblokpoel.lanag.util.RNG
import org.apache.spark.SparkContext

@SerialVersionUID(100L)
case class MutationMatrix(vocabularySize: Int,
                          contextSize: Int,
                          order: Int,
                          k: Int,
                          sampleSize: Int,
                          allPossibleAgents: List[RSA1ShotAgent],
                          q: List[List[Double]]) {
  def apply(speaker: RSA1ShotAgent, learner: RSA1ShotAgent): Double =
    q(allPossibleAgents.indexOf(speaker))(allPossibleAgents.indexOf(learner))

  def learnerDistribution(speaker: RSA1ShotAgent): List[Double] =
    q(allPossibleAgents.indexOf(speaker))

  def posterior(speakerPriors: Distribution[RSA1ShotAgent])
    : Distribution[RSA1ShotAgent] = {
    assert(speakerPriors.length == allPossibleAgents.length)

    @scala.annotation.tailrec
    def posteriorRec(
        speakerPriors: Distribution[RSA1ShotAgent],
        speakerId: Int,
        q: List[List[Double]],
        posterior: Vector[BigDecimal]): Distribution[RSA1ShotAgent] = q match {
      case Nil => Distribution(allPossibleAgents.toVector, posterior)
      case head :: tail =>
        val speaker = allPossibleAgents(speakerId)
        val partialPosterior = head.map(_ * speakerPriors(speaker))
        val newPosterior =
          (posterior zip partialPosterior).map(p => p._1 + p._2)
        posteriorRec(speakerPriors, speakerId + 1, tail, newPosterior)
    }

    posteriorRec(
      speakerPriors,
      speakerId = 0,
      q,
      Vector.tabulate[BigDecimal](allPossibleAgents.length)(_ =>
        BigDecimal(0.0))
    )
  }
}

case object MutationMatrix {
  type AgentPair = (RSA1ShotAgent, RSA1ShotAgent)
  type Datum = Seq[Seq[(ContentSignal, ReferentialIntention)]]

  private def mutate(signal: ContentSignal, vocabularySize: Int): ContentSignal = {
    if(signal.isDefined) {
      val signalId = signal.content.get
      val randomSignalId = (signalId + 1 + RNG.nextInt(vocabularySize - 1)) % vocabularySize
      ContentSignal(randomSignalId)
    } else {
      ContentSignal(None)
    }
  }

  def apply(vocabularySize: Int,
            contextSize: Int,
            order: Int,
            sparkContext: SparkContext,
            k: Int = 5,
            sampleSize: Int = 10,
            errorRate: Double = 0.0): MutationMatrix = {
    val allPossibleAgents: List[RSA1ShotAgent] =
      allBooleanPermutations(contextSize * vocabularySize)
        .map(permutation => Lexicon(vocabularySize, contextSize, permutation))
        .filter(lexicon => lexicon.isConsistent)
        .map(lexicon => new RSA1ShotAgent(lexicon, order))

    val datum: Datum = for (_ <- 0 until sampleSize)
      yield generateObservations(vocabularySize, contextSize, k)

    val allPossibleAGentBroadcast = sparkContext.broadcast(allPossibleAgents)
    val datumBroadcast = sparkContext.broadcast(datum)

    val q = sparkContext
      .parallelize(allPossibleAgents)
      .map(speaker => {
        for (learner <- allPossibleAGentBroadcast.value) yield {
          datumBroadcast.value
            .map(d => {
              d.map(obs => {
                  val (signal, intention) =
                    if(RNG.nextProbability < errorRate) obs
                    else (mutate(obs._1, vocabularySize), obs._2)
                  val speakerP =
                    speaker.asSpeaker.inferredLexicon(signal.content.get,
                                                      intention.content.get)
                  val learnerP =
                    learner.asSpeaker.inferredLexicon(signal.content.get,
                                                      intention.content.get)
                  speakerP * learnerP
                })
                .foldLeft(1.0)(_ * _)
            })
            .sum
        }
      })
      // normalize across speakers
      .map(speakerRow => {
        val total = speakerRow.sum
        if (total > 0) speakerRow.map(_ / total)
        else speakerRow.map(_ => 0.0)
      })
      .toLocalIterator
      .toList

    MutationMatrix(vocabularySize,
                   contextSize,
                   order,
                   k,
                   sampleSize,
                   allPossibleAgents,
                   q)
  }

  private def allBooleanPermutations(length: Int): List[Vector[Double]] = {
    if (length == 1) List(Vector(1.0), Vector(0.0))
    else {
      val outp = for (lex <- allBooleanPermutations(length - 1))
        yield List(Vector(1.0) ++ lex, Vector(0.0) ++ lex)
      outp.flatten
    }
  }

  private def generateObservations(
      vocabularySize: Int,
      contextSize: Int,
      k: Int): Seq[(ContentSignal, ReferentialIntention)] =
    for (_ <- 0 until k)
      yield
        (ContentSignal(RNG.nextInt(vocabularySize)),
         ReferentialIntention(RNG.nextInt(contextSize)))

}
