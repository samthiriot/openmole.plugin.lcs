/*
 * Copyright (C) 2018 Samuel Thiriot
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

package org.openmole.plugin.method.microlcs

import org.openmole.core.context.{ Val, Variable }
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.tool.logger.JavaLogger

import scala.annotation.tailrec

/**
 * Takes a set of rules, and merges them when possible.
 */
object Subsumption extends JavaLogger {

  @tailrec
  def absorbSubsumed(r: ClassifierRule, epsilons: Array[Double], rules: List[ClassifierRule], acc: List[ClassifierRule]): (ClassifierRule, List[ClassifierRule]) = rules match {
    case Nil ⇒
      //System.out.println("finished processing " + r + " and returning " + acc.length + " rules")
      (r, acc)
    case r2 :: tail ⇒
      //System.out.println("comparing rules(" + tail.length + " remaining):\n\t" + r + "\n\t" + r2)

      if (r eq r2) {
        //System.out.println("its the same !")
        absorbSubsumed(r, epsilons, tail, acc)
      }
      else if ((r.sameActions(r2)) && (r.sameConditions(r2) || (r.subsums(r2) && r.similarPerformance(r2, epsilons)))) {
        //System.out.println("comparing rules(" + tail.length + " remaining):\n\t" + r + "\n\t" + r2)
        //System.out.println("=> subsuming or the same")
        // absorb r2 into r
        //System.out.println("absorb r2 into 2...")
        r.absorb(r2)
        // continue, but do not consider the absorbed rule anymore
        //System.out.println("continuing recursively to process the " + tail.length + " remaining elements (" + acc.length + " accumulated)")
        absorbSubsumed(r, epsilons, tail, acc)

      }
      else {
        //System.out.println("continuing")
        absorbSubsumed(r, epsilons, tail, acc ::: List(r2))
      }

  }

  def absorbSubsumed(r: ClassifierRule, epsilons: Array[Double], rules: List[ClassifierRule]): (ClassifierRule, List[ClassifierRule]) = absorbSubsumed(r, epsilons, rules, List())

  /**
   * We browse a list of rules like [A, B, C, D, E, F]
   * At a step we study [A, B, C] [D, E, F]; we study how D is able to absorb rules before or rules after
   * @param rulesAfter
   * @param rulesBefore
   * @return
   */
  @tailrec
  def compareRules(
    epsilons:    Array[Double],
    rulesAfter:  List[ClassifierRule],
    rulesBefore: List[ClassifierRule] = List()
  ): List[ClassifierRule] = rulesAfter match {
    case Nil ⇒ rulesAfter ::: rulesBefore
    case r :: tail ⇒
      //System.out.println("working on rule " + r)
      //System.out.println("subsumption of the " + rulesBefore.length + " rules before")
      val (r2, rulesBeforeUpdated) = absorbSubsumed(r, epsilons, rulesBefore)
      //System.out.println("subsumption of the " + tail.length + " rules after")
      val (r3, tailUpdated) = absorbSubsumed(r2, epsilons, tail)
      compareRules(epsilons, tailUpdated, rulesBeforeUpdated ::: List(r3))
  }

  def apply(
    microMinimize: Seq[Val[Double]],
    microMaximize: Seq[Val[Double]],
    maxIteration:  Int,
    similarity:    Int              = 100,
    verbose:       Boolean          = false
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("Subsumption") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)

      System.out.println("Applying subsumption on " + rules.length + " rules"
      // + rules.map(_.name).mkString(",")
      )
      
      val mins = context(varMin)
      val maxs = context(varMax)
      val iteration = context(varIterations)

      if (verbose)
        System.out.println("simplifying rules based on mins and maxs: " + mins.mkString(",") + " " + maxs.mkString(","))

      val rulesWithoutDoubles = rules.toSet
      val rulesSimplified = rulesWithoutDoubles.map(ClassifierRule.simplify(_, mins, maxs))
      val rulesShuffled = rulesSimplified.toList.sortBy(_.id).toArray
      //rng().shuffle(rulesWithoutDoubles.toList).toArray

      val simulationsCount = context(varSimulationCount)

      if (verbose)
        System.out.println("Applying subsumption on " + rulesShuffled.length + " unique rules " + rulesShuffled.map(_.name).mkString(","))

      val minPerIndicator: Array[Double] = (0 to microMinimize.length + microMaximize.length - 1).map(
        i ⇒ rulesShuffled.map(
          r ⇒ r.min(i)
        ).min).toArray
      val maxPerIndicator: Array[Double] = (0 to microMinimize.length + microMaximize.length - 1).map(
        i ⇒ rulesShuffled.map(
          r ⇒ r.max(i)
        ).max).toArray

      val epsilons = (minPerIndicator zip maxPerIndicator).map { case (min, max) ⇒ (max - min) / similarity.toDouble }

      if (verbose)
        System.out.println("Using epsilons on performance to define whether two rules can be merged or not:\n" +
          (microMinimize ++ microMaximize).zipWithIndex
          .map { case (indic, i) ⇒ indic.simpleName + " [" + minPerIndicator(i) + ":" + maxPerIndicator(i) + "] => " + epsilons(i) }
          .mkString(",\n")
        )

      val rulesUpdated = compareRules(epsilons, rulesShuffled.toList)

      if (verbose)
        System.out.println("\nMicro iteration " + iteration + "/" + maxIteration + " - rules after subsumption (capitalizing " +
          rulesUpdated.map(r ⇒ r.applications).sum + " micro simulations - over " + simulationsCount + " ran total):\n" +
          ClassifierRule.toPrettyString(rulesUpdated)
        )

      System.out.println("Subsumption reduced rules from " + rulesShuffled.length + " to " + rulesUpdated.length + " rules")

      List(
        Variable(varRules, rulesUpdated.toArray)
      )

    } set (
      // we expect as inputs:
      // ... the rules we used last time
      inputs += varRules,

      // we provide as outputs
      // ... the rules we updates with the novel information
      outputs += varRules,

      (inputs, outputs) += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,
      (inputs, outputs) += varMin,
      (inputs, outputs) += varMax,
      (inputs, outputs) += varSimulationCount
    )

  }
}