/*
 * Copyright (C) 2018 Samuel Thiriot
 *                    Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.openmole.plugin.method

import org.openmole.core.context.{ Val, _ }
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.execution.EnvironmentProvider
import org.openmole.core.workflow.mole._
import org.openmole.core.workflow.puzzle._
import org.openmole.core.workflow.task._
import org.openmole.core.workflow.transition.Slot
import org.openmole.core.workspace.NewFile
import org.openmole.tool.random.RandomProvider

package object microlcs {

  val namespaceMicroLCS = Namespace("microlcs")

  // this value will contain the set of rules
  val varRules = Val[Array[ClassifierRule]]("rules", namespace = namespaceMicroLCS)
  // refers to a list of rules applied on a simultion, each rule corresponding one entity
  val varRulesApplied = Val[Array[ClassifierRule]]("rules", namespace = namespaceMicroLCS)

  val varIterations = Val[Int]("iterations", namespace = namespaceMicroLCS)

  val varPlans = Val[Array[MacroGene]]("plans", namespace = namespaceMicroLCS)
  val varPlansBefore = Val[Array[MacroGene]]("plans_before", namespace = namespaceMicroLCS)

  val varPlanSimulated = Val[MacroGene]("plan_simulated", namespace = namespaceMicroLCS)

  // traces how many simulations were run total
  val varSimulationCount = Val[Int]("simulations", namespace = namespaceMicroLCS)

  implicit def scope = DefinitionScope.Internal

  def weightedWheel[T](elems: Array[T], weights: Array[Double])(rng: RandomProvider): T = {

    val total: Double = weights.sum
    val r: Double = rng().nextDouble() * total

    var cumulated = 0.0
    var i = 0
    while ((i < elems.length - 1) && (cumulated < r)) {
      cumulated += weights(i)
      i += 1
    }
    elems(i)
  }

  case class MicroCharacteristic(prototype: Val[Array[T]] forSome { type T })

  object MicroCharacteristic {

    implicit def valDoubleIsMicroCharacteristic(vd: Val[Array[Double]]) = MicroCharacteristic(vd)
    implicit def valIntIsMicroCharacteristic(vi: Val[Array[Int]]) = MicroCharacteristic(vi)
    implicit def valBooleanIsMicroCharacteristic(vb: Val[Array[Boolean]]) = MicroCharacteristic(vb)
    implicit def valStringIsMicroCharacteristic(vs: Val[Array[String]]) = MicroCharacteristic(vs)

    //implicit def microCharacteristicIsVal(m: MicroCharacteristic): Val[Array[T]] forSome { type T } = m.prototype

  }

  type MicroCharacteristics = Seq[MicroCharacteristic]

  // TODO MacroLCS

  /**
   * Entry point for the method: applies MicroLCS
   * with a list of input characteristics for entities,
   * actions to tune for each entity, and
   * a count of iterations to drive.
   */
  def MicroLCS(
    microCharacteristics: MicroCharacteristics, // Seq[Val[Array[T]] forSome { type T }],
    microActions:         Seq[MicroGenes.Gene[_]],
    iterations:           Int,
    evaluation:           Puzzle,
    environment:          EnvironmentProvider,
    microMinimize:        Seq[Val[Double]],
    microMaximize:        Seq[Val[Double]],
    count:                Int                     = 200,
    similarity:           Int                     = 100
  //macroMinimize:        Seq[Val[Double]],
  //macroMaximize:        Seq[Val[Double]]
  )(implicit newFile: NewFile, fileService: FileService): Puzzle = {

    //
    // rng: RandomProvider,

    val simulation = evaluation // Capsule(MoleTask(evaluation))

    // the first step is to decode the initial lists of characteristics as lists of individuals.
    val decodeIndividuals = DecodeEntities(microCharacteristics, microActions)
    val sDecodeIndividuals = Slot(decodeIndividuals)

    val doMatching = Matching(microActions, false)
    val cDoMatching = Capsule(doMatching)
    val sDoMatching = Slot(cDoMatching) on environment

    val encodeIndividuals = EncodeEntities(microCharacteristics, microActions)
    val sEncodeIndividuals = Slot(encodeIndividuals) on environment

    val evaluate = Evaluate(microMinimize, microMaximize)
    val sEvaluate = Slot(evaluate) on environment

    val subsume = Subsumption(microMinimize, microMaximize, iterations, similarity)

    val evolve = Evolve(microActions, microCharacteristics, count)
    val sEvolve = Slot(evolve)

    val delete = Delete(count * 2)
    val cDelete = Capsule(delete)
    val sDelete = Slot(cDelete)

    //val sDoMatchingLoop = Slot(cDoMatching)

    val dispatch = ExplorationTask(DispatchEntities(16))

    val aggregate = AggregateResults()

    val beginLoop = EmptyTask() set (
      name := "beginLoop",
      (inputs, outputs) += (DecodeEntities.varEntities, varRules, varIterations, DecodeEntities.varMin, DecodeEntities.varMax)
    )

    val beginLoopCapsule = Capsule(beginLoop, strain = true)
    val beginLoopExecInit = Slot(beginLoopCapsule)
    val beginLoopExecLoop = Slot(beginLoopCapsule)

    val export = ExportRules(microCharacteristics, microActions, microMinimize, microMaximize)
    val exportSlot = Slot(export)

    val last = EmptyTask() set (
      name := "last" //,
    //(inputs, outputs) += (t.populationPrototype, t.statePrototype)
    )
    val lastCapsule = Capsule(last, strain = true)

    (
      (sDecodeIndividuals -- beginLoopExecInit -- dispatch
        -< (sDoMatching -- sEncodeIndividuals -- simulation -- sEvaluate) >-
        aggregate -- subsume -- sEvolve -- sDelete) &
        // convey rules, iteration, micro entities and other information over the evaluation
        (sEncodeIndividuals -- sEvaluate) &
        // loop
        (sDelete -- (beginLoopExecLoop when "microlcs$iterations < " + iterations)) &
        // last step: run exportation
        (sDelete -- (exportSlot when "microlcs$iterations == " + iterations))
    ) -- (lastCapsule when "microlcs$iterations == " + iterations)

  }

  /**
   * Entry point for the method: applies MicroLCS
   * with a list of input characteristics for entities,
   * actions to tune for each entity, and
   * a count of iterations to drive.
   */
  def DiscoverPlansLCS(
    microCharacteristics: MicroCharacteristics, // Seq[Val[Array[T]] forSome { type T }],
    microActions:         Seq[MicroGenes.Gene[_]],
    iterations:           Int,
    evaluation:           Puzzle,
    environment:          EnvironmentProvider,
    microMinimize:        Seq[Val[Double]],
    microMaximize:        Seq[Val[Double]],
    macroMinimize:        Seq[Val[Double]],
    macroMaximize:        Seq[Val[Double]],
    proportions:          Seq[Double]             = Seq(0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
    count:                Int                     = 100
  )(implicit newFile: NewFile, fileService: FileService): Puzzle = {

    val generateInitPlans = GenerateInitPlans(microMinimize, microMaximize, proportions, count)
    val generateInitPlansSlot = Slot(generateInitPlans)

    val dispatchPlans = ExplorationTask(SamplePlans())

    val matchingPlans = Matching(microActions, true) set ((inputs, outputs) += (varPlanSimulated, varPlansBefore)) on environment
    val encodeIndividualsPlans = EncodeEntities(microCharacteristics, microActions) set ((inputs, outputs) += (varPlanSimulated, varPlansBefore))
    val sEncodeIndividualsPlans = Slot(encodeIndividualsPlans) on environment

    val evaluatePlan = EvaluateMacro(microMinimize, microMaximize, macroMinimize, macroMaximize)
    val sEvaluatePlan = Slot(evaluatePlan) on environment

    val aggregatePlans = AggregateResultsPlan()

    val evolvePlans = EvolvePlans(count, microActions, proportions, iterations)
    val sEvolvePlans = Slot(evolvePlans)

    val beginLoop = Capsule(EmptyTask() set (name := "begin loop"), strain = true)
    val beginLoopInit = Slot(beginLoop)
    val beginLoopRepeat = Slot(beginLoop)

    (
      (generateInitPlansSlot -- beginLoopInit -- dispatchPlans
        -< (matchingPlans -- sEncodeIndividualsPlans -- evaluation -- sEvaluatePlan) >-
        aggregatePlans -- sEvolvePlans) &
        // pass data over the simulation of macro plans
        (sEncodeIndividualsPlans -- sEvaluatePlan) &
        // loop for the evolution of macro plans
        (sEvolvePlans -- (beginLoopRepeat when "microlcs$iterations < " + iterations))

    ) // TODO !!! -- export

  }

}

