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
import org.openmole.tool.random.RandomProvider

import scala.annotation.tailrec

/**
 * Takes rules and entities,
 * and elaborates the initial plans to explore
 */
object GenerateInitPlans extends JavaLogger {

  def takeNRules(n: Int, rules: Array[ClassifierRule]): Array[ClassifierRule] = {
    if (rules.length <= n) {
      rules
    }
    else {
      val sk = rules.length / n
      rules.zipWithIndex
        .filter { case (c, i) ⇒ (i % n == sk) }
        .map { case (c, i) ⇒ c }
    }
  }

  def sortByNthMicroPerf(i: Int, rules: Array[ClassifierRule]): Array[ClassifierRule] =
    rules.sortWith(
      (r1, r2) ⇒ r1.performanceAggregated(i) < r2.performanceAggregated(i)
    )

  def biasedWheelForRule(rules: Array[ClassifierRule])(rng: RandomProvider): ClassifierRule = {
    val randd = rng().nextDouble()
    val idx = math.floor((1 - math.sin(1.55 * randd + math.Pi / 2)) * rules.length).toInt
    //  TODO check System.out.println("idx: " + idx + "/" + rules.length)
    rules(math.min(idx, rules.length))
  }

  @tailrec
  def simplifyRules(rules: List[ClassifierRule], acc: List[ClassifierRule] = List()): Array[ClassifierRule] = rules match {
    case Nil ⇒ acc.toArray
    case r :: tail ⇒
      if (tail.exists(o ⇒ r.sameActions(o) && o.subsums(r))) {
        // this rule is subsumed by another rule with similar actions; let's ignore it
        simplifyRules(tail, acc)
      }
      else {
        // this rule is not subsumed; let's keep it
        simplifyRules(tail, acc :+ r)
      }
  }

  def elaboratePlan(
    id:                Int,
    rulesSelected:     Array[ClassifierRule],
    rulesAll:          Array[ClassifierRule],
    entitiesToProcess: List[Entity],
    proportions:       Seq[Double]
  )(implicit rng: RandomProvider, fileService: FileService): MacroGene = entitiesToProcess match {

    case Nil ⇒ // end of process

      val rulesWithRandomProportions =
        rulesSelected.slice(0, rulesSelected.length - 1)
          .map(ClassifierRule.mutateProportion(_, proportions)) :+ rulesSelected(rulesSelected.length - 1)

      val rulesWithoutDouble = simplifyRules(rulesWithRandomProportions.toList)

      MacroGene(id, rulesWithoutDouble)

    case e :: entities ⇒
      val rulesMatching = rulesAll.filter(r ⇒ r.matches(e))
      if (rulesMatching.isEmpty) {
        throw new RuntimeException("unable to find a rule to match entity " + e + ", this is not theoretically possible!")
      }

      val ruleToUse = if (rulesMatching.length == 1) {
        // only one possibility, let's use it ^^
        rulesMatching(0)
      }
      else {
        // several rules match this entity

        /*
        rulesMatching.foreach(
          r ⇒ System.out.println(
            "rule " + r + " => distance actions: " + rulesSelected.map(r.distanceActions(_)).sum + " \t generality :" + r.generalityIndice()
          )
        )*/

        // we order them according to their difference of actions and also decreasing generality
        val rulesSortedByOtherActionsAndGenericity = rulesMatching.sortWith(
          (r1, r2) ⇒ r1.generalityIndice() > r2.generalityIndice()
        ).sortWith(
            (r1, r2) ⇒ rulesSelected.map(r1.distanceActions(_)).sum > rulesSelected.map(r2.distanceActions(_)).sum
          )

        // TODO biased wheel
        /*
        weightedWheel(
          rulesSortedByOtherActionsAndGenericity,
          (rulesSortedByOtherActionsAndGenericity.length - 1 to 0 by -1).map(_ * 2.0).toArray
        )*/
        rulesSortedByOtherActionsAndGenericity(0)
        //biasedWheelForRule(rulesSortedByOtherActionsAndGenericity)(rng)

      }

      // recusrively continue to build the plan
      elaboratePlan(
        id,
        rulesSelected ++ Array(ruleToUse),
        rulesAll,
        entitiesToProcess.filterNot(ruleToUse.matches(_)),
        proportions
      )

  }

  /**
   * Takes a given rule, and builds a
   */
  def elaboratePlanAroundARule(
    id:          Int,
    rule:        ClassifierRule,
    rules:       Array[ClassifierRule],
    entities:    Array[Entity],
    proportions: Seq[Double]
  )(implicit rng: RandomProvider, fileService: FileService): MacroGene = elaboratePlan(
    id,
    Array(rule),
    rules,
    entities.filter(e ⇒ !rule.matches(e)).toList,
    proportions
  )

  def apply(
    microMinimize: Seq[Val[Double]],
    microMaximize: Seq[Val[Double]],
    proportions:   Seq[Double],
    maxrules:      Int
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("GenerateInitPlans") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the current iteration

      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)
      val entities: Array[Entity] = context(DecodeEntities.varEntities)

      Log.log(Log.FINE, "Generating the " + maxrules + " initial plans to be explored")

      // keep only the rules which have been tested
      val rulesTested: Array[ClassifierRule] = rules.filter(r ⇒ (r.applications > 0))

      // also, only keep the n best Pareto optimal solutions
      val rulesRankedPareto: Seq[Iterable[ClassifierRule]] = HasMultiObjectivePerformance.detectParetoFronts(rulesTested)
      //val rulesRankedParetoBest = rulesRankedPareto.take(1)

      //System.out.println("\n\n" + HasMultiObjectivePerformance.paretoFrontsToPrettyString(rulesRankedPareto))

      // only keep max N of these solutions
      val rulesFiltered: Array[ClassifierRule] = HasMultiObjectivePerformance.selectParentsFromFronts(maxrules, rulesRankedPareto.toList)(rng).toArray

      // so rulesFiltered contains only the best of the best rules

      val countPerMicro = maxrules / (microMinimize.length + microMaximize.length)

      val plans: Array[MacroGene] =
        (microMinimize ++ microMaximize)
          .zipWithIndex
          .flatMap {
            case (t, i) ⇒ takeNRules(
              countPerMicro,
              sortByNthMicroPerf(i, rulesFiltered)
            ).zipWithIndex
              .map {
                case (r, j) ⇒ elaboratePlanAroundARule(i * (countPerMicro + 1) + j, r, rulesFiltered.reverse, entities, proportions)(rng, fileService)
              }
          }.toArray

      // TODO eliminate doubles

      Log.log(Log.INFO, "Here are the initial plans to be explored:\n\n" + MacroGene.toPrettyString(plans))
      System.out.println("Here are the initial plans to be explored:\n\n" + MacroGene.toPrettyString(plans))

      List(
        Variable(varRules, rulesFiltered),
        Variable(varPlans, plans),
        Variable(varIterations, 1),
        Variable(varPlansBefore, Array[MacroGene]())
      )

    } set (
      // we expect as inputs:
      // ... the rules we used for each entity
      inputs += varRules,

      // we provide as outputs
      //outputs += DecodeEntities.varEntities,
      // ... the rules we updates with the novel information
      outputs += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,
      outputs += varPlans,
      outputs += varRules,
      outputs += varPlansBefore,

      (inputs, outputs) += varMin,
      (inputs, outputs) += varMax,
      (inputs, outputs) += varSimulationCount

    )

  }
}