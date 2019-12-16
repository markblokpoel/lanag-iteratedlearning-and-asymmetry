package com.markblokpoel.lanag.ilasymmetry

import com.markblokpoel.lanag.ambiguityhelps.RSA1ShotAgent
import com.markblokpoel.lanag.core.{ContentSignal, ReferentialIntention}
import com.markblokpoel.lanag.math.Distribution
import com.markblokpoel.lanag.rsa.Lexicon
import com.markblokpoel.lanag.util.{RNG, SparkSimulation, StreamingMap}

case class TransitionMatrix(vocabularySize: Int,
                            contextSize: Int,
                            order: Int,
                            k: Int,
                            sampleSize: Int,
                            errorRate: Double,
                            sm: SparkSimulation) {
  require(order >= 0,
          s"order must be greater than or equal to 0, but is $order.")
  require(vocabularySize > 0,
          s"vocabularySize must be greater than 0, but is $vocabularySize.")
  require(contextSize > 0,
          s"contextSize must be greater than 0 but is $contextSize.")
  require(k > 0, s"k must be greater than 0, but is $k")
  require(sampleSize > 0,
          s"sampleSize must be greater than 0 but is $sampleSize.")
  require(
    errorRate >= 0 && errorRate <= 1.0,
    s"errorRate must be between 0.0 and 1.0 (inclusive), but is $errorRate.")

  type Datum = Seq[Seq[(ContentSignal, ReferentialIntention)]]

  val length: Int = math.pow(2, (vocabularySize * contextSize).toDouble).toInt

  private val lexiconIndices = 0 until length

  val allPossibleLexicons: Vector[Lexicon] =
    lexiconIndices.map(idxToLexicon).toVector

  private val allPossibleLexiconsBr = sm.context.broadcast(allPossibleLexicons)

  val allPossibleAgents: Vector[RSA1ShotAgent] =
    allPossibleLexicons.map(new RSA1ShotAgent(_, order))

  def transitions(speakerIdx: Int): Vector[Double] = {
    sm.parallelize(allPossibleAgents)
      .map(learnerAgent => {
        val speaker =
          new RSA1ShotAgent(allPossibleLexiconsBr.value(speakerIdx), order)
        val learner = learnerAgent
        generateObservations(speaker, k, sampleSize)
          .map(observations => prob(observations, speaker, learner))
          .sum
      })
      .collect()
      .toVector
  }

  val q: StreamingMap[Int, Vector[Double]] =
    new StreamingMap[Int, Vector[Double]](lexiconIndices, transitions)

  val zeroDistribution: Distribution[RSA1ShotAgent] =
    Distribution(allPossibleAgents, Vector.tabulate(length)(_ => BigDecimal(0)))

  def apply(speaker: RSA1ShotAgent, learner: RSA1ShotAgent): Double =
    q(lexiconToIdx(speaker.originalLexicon))(
      lexiconToIdx(learner.originalLexicon))

  def getLearnerDistribution(
      speaker: RSA1ShotAgent): Distribution[RSA1ShotAgent] = {
    val d = q(lexiconToIdx(speaker.originalLexicon)).map(BigDecimal(_))
    Distribution[RSA1ShotAgent](allPossibleAgents, d)
  }

  def idxToLexiconInit(idx: Int): Lexicon = {
    val str = idx.toBinaryString
    val bits = (0 until vocabularySize * contextSize - str.length)
      .map(_ => "0")
      .foldLeft("")(_ + _) + str
    Lexicon(vocabularySize,
            contextSize,
            bits.map(b => Integer.parseInt(b.toString).toDouble).toVector,
      Lexicon.asBlokpoeletalSpeaker,
      Lexicon.asBlokpoelEtalListener)
  }

  def idxToLexicon(idx: Int): Lexicon = {
    require(0 <= idx && idx <= length,
      s"Lexicon idx out of range, $idx <0 or > $length.")
    allPossibleLexicons(idx)
  }

  def lexiconToIdx(lexicon: Lexicon): Int = {
    require(lexicon.data.forall(v => v == 0.0 || v == 1.0),
            s"lexicon is graded, cannot convert to binary string.")
    allPossibleLexicons.indexOf(lexicon)
//    Integer.parseInt(lexicon.data.map(_.intValue).mkString(""), 2)
  }

  private def generateObservations(
      speaker: RSA1ShotAgent,
      k: Int,
      sampleSize: Int): Seq[Seq[(ContentSignal, ReferentialIntention)]] = {
    for (_ <- 0 until sampleSize)
      yield
        for (_ <- 0 until k) yield {
          val randomIntention = ReferentialIntention(
            RNG.nextInt(speaker.originalLexicon.contextSize))
          val (signal, _) = speaker.asSpeaker.produceSignal(randomIntention)
          (signal, randomIntention)
        }
  }

  private def mutate(signal: ContentSignal,
                     vocabularySize: Int): ContentSignal = {
    if (signal.isDefined) {
      val signalId = signal.content.get
      val randomSignalId = (signalId + 1 + RNG.nextInt(vocabularySize - 1)) % vocabularySize
      ContentSignal(randomSignalId)
    } else {
      ContentSignal(None)
    }
  }

  private def prob(observations: Seq[(ContentSignal, ReferentialIntention)],
                   speaker: RSA1ShotAgent,
                   learner: RSA1ShotAgent): Double =
    observations
      .map(obs => {
        val (signal, intention) =
          if (RNG.nextProbability > errorRate) obs
          else (mutate(obs._1, vocabularySize), obs._2)
        val speakerP = speaker.asSpeaker.inferredLexicon(signal.content.get,
                                                         intention.content.get)
        val learnerP = learner.asSpeaker.inferredLexicon(signal.content.get,
                                                         intention.content.get)
        speakerP * learnerP
      })
      .foldLeft(1.0)(_ * _)
}
