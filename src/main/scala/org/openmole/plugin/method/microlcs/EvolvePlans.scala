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

import org.openmole.core.context.Variable
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.tool.logger.JavaLogger

import scala.annotation.tailrec

/**
 * ensures we only keep a given maximum of rules
 */
object EvolvePlans extends JavaLogger {

  def rulesEquivalent(p: MacroGene, q: MacroGene): Boolean =
    p.rules.length == q.rules.length &&
      (p.rules zip q.rules).forall {
        case (a, b) ⇒ (
          (a.name == b.name) || (
            (a.proportion == b.proportion) && (a.sameConditions(b)) && (a.sameActions(b))
          ))
      }

  @tailrec
  def groupSimilar(plans: List[MacroGene], acc: List[MacroGene] = List()): List[MacroGene] = plans match {
    case Nil ⇒ acc
    case p :: tail ⇒
      val doublesFromTail = tail.filter(rulesEquivalent(p, _))
      if (!doublesFromTail.isEmpty) {
        //System.out.println("found " + doublesFromTail.length + " doubles for plan\n" + p)
      }
      val pUpdated = (List(p) ++ doublesFromTail).reduceLeft(_.absorb(_))
      if (!doublesFromTail.isEmpty) {
        Log.log(Log.FINER, "absorbing " + doublesFromTail.map(_.name).mkString(",") + " into " + p.name + "\n" +
          "p updated =>\n" + pUpdated)
      }
      val tailUpdated = tail diff doublesFromTail
      /*if (!doublesFromTail.isEmpty) {
        System.out.println("tail from " + tail.length + " to " + tailUpdated.length)
      }*/
      groupSimilar(tailUpdated, acc ++ List(pUpdated))
  }

  def apply(
    maxrules:     Int,
    microActions: Seq[MicroGenes.Gene[_]],
    proportions:  Seq[Double],
    maxIteration: Int
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("EvolvePlans") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the current iteration
      val iteration: Int = context(varIterations)

      val countEntities: Int = context(DecodeEntities.varEntities).length

      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)

      val plans: Array[MacroGene] = context(varPlans)
      val plansBefore: Array[MacroGene] = context(varPlansBefore)

      //System.out.println("Iteration " + iteration + " Here are the " + plans.length + " plans after evaluation:\n" + MacroGene.toPrettyString(plans))

      //val plansUnique = groupSimilar(plans.toList)

      // elitism: we conserve the best of the previous as well !
      //val bestPlans = HasMultiObjectivePerformance.detectParetoFront(plansUnique)

      //System.out.println("Pareto optimal plans:\n" + MacroGene.toPrettyString(bestPlans.toList.sortWith(_.performanceAggregated(0) < _.performanceAggregated(0))))

      val parents = (plans.toList ++ plansBefore).toSet.toList
      val parentsUnique = groupSimilar(parents)

      val parentsRankedPareto = HasMultiObjectivePerformance.detectParetoFronts(parentsUnique.toArray)

      val simulationsCount = context(varSimulationCount)

      Log.log(Log.INFO, "Iteration " + iteration + "/" + maxIteration + " - we have: " + parents.length + " parents, " +
        "then " + parentsUnique.length + " unique parents " +
        "evaluating over " + parentsRankedPareto.length + " Pareto fronts" +
        " (total " + simulationsCount + " simulations)\n\n" +
        HasMultiObjectivePerformance.paretoFrontsToPrettyString(parentsRankedPareto.take(5)))

      System.out.println(
        "Macro iteration " + iteration + "/" + maxIteration + " - here are the three first Pareto fronts:\n" +
        HasMultiObjectivePerformance.paretoFrontsToPrettyString(parentsRankedPareto.take(3)))

      // select n parents; they will be taken from the first front, then next, then next, etc...
      val parentsSelected: List[MacroGene] = HasMultiObjectivePerformance.selectParentsFromFronts(maxrules, parentsRankedPareto.toList)(rng).toList

      // we can do crossover between rules which are of similar size
      // TODO crossover

      val mins: Array[Double] = context(DecodeEntities.varMin)
      val maxs: Array[Double] = context(DecodeEntities.varMax)

      // TODO try again some of the best which were not tested that much
      val goodPlansToTestAgain: Array[MacroGene] = parentsRankedPareto(0).filter(_.applications <= 5).toArray

      // mutate !
      val plansMutated: Array[MacroGene] = (0 to maxrules - 1).map(i ⇒ parentsSelected(i % parentsSelected.length)).map(
        p ⇒
          if (p.rules.length > 1)
            MacroGene.mutate(
            p,
            microActions,
            mins,
            maxs,
            countEntities,
            proportions,
            context
          )(rng, fileService)
          else
            p
      ).toArray
      /*
      val plansMutated = parentsSelected.map(
        p ⇒
          if (p.rules.length > 1)
            MacroGene.mutate(
            p,
            microActions,
            mins,
            maxs,
            countEntities,
            proportions,
            context
          )(rng, fileService)
          else
            p
      ).toArray
      */

      List(
        Variable(varPlans, goodPlansToTestAgain ++ plansMutated),
        Variable(varPlansBefore, parentsRankedPareto(0).toArray)
      )

    } set (
      // the plans are taken as inputs, and evolved before being outputed
      (inputs, outputs) += varPlans,
      // the rules we used for each entity
      (inputs, outputs) += varRules,
      (inputs, outputs) += varPlansBefore,

      (inputs, outputs) += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,
      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax,
      (inputs, outputs) += varSimulationCount

    )

  }
}