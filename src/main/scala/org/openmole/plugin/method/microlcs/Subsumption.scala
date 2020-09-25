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
      //Log.log(Log.INFO, "\tabsorbSubsumed: r:"+r.name+" rules:"+rules.map(_.name).mkString(",")+" acc:"+acc.map(_.name).mkString(","))

      //System.out.println("comparing rules(" + tail.length + " remaining):\n\t" + r + "\n\t" + r2)

      if (r eq r2) {
        // they are the same! 
        // let's ignore r2 
        Log.log(Log.INFO, "\tsame rules: keeping "+r.name+" and forgeting "+r2.name)
        absorbSubsumed(r, epsilons, tail, acc)
      }
      else if ((r.sameActions(r2)) && (r.sameConditions(r2) || (r.subsums(r2) && r.similarPerformance(r2, epsilons)))) {
        //System.out.println("comparing rules(" + tail.length + " remaining):\n\t" + r + "\n\t" + r2)
        //System.out.println("=> subsuming or the same")
        // absorb r2 into r

        if (r.id < r2.id) {
          //Log.log(Log.INFO, "\tabsorbing "+r2.name+" into "+r.name)
          // continue, but do not consider the absorbed rule anymore
          //System.out.println("continuing recursively to process the " + tail.length + " remaining elements (" + acc.length + " accumulated)")
          absorbSubsumed(r.absorb(r2), epsilons, tail, acc)
        } else {
          //Log.log(Log.INFO, "\tabsorbing "+r.name+" into "+r2.name)
          // continue, but do not consider the absorbed rule anymore
          //System.out.println("continuing recursively to process the " + tail.length + " remaining elements (" + acc.length + " accumulated)")
          absorbSubsumed(r2.absorb(r), epsilons, tail, acc)
        }
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

      System.out.println("Applying subsumption on " + rules.length + " rules")
      
      val mins = context(varMin)
      val maxs = context(varMax)
      val iteration = context(varIterations)

      if (verbose) {
        System.out.println("\tsimplifying rules based on mins "+mins.mkString(",")+" and maxs: "+ maxs.mkString(","))
      }
    
      val rulesWithoutDoubles = rules.toSet
      val rulesSimplified = rulesWithoutDoubles.map(ClassifierRule.simplify(_, mins, maxs))
      val rulesShuffled = rulesSimplified.toList.sortBy(_.id).toArray // sort by ID so we keep a preference, when two rules are equivalent, for the older ones
      //rng().shuffle(rulesWithoutDoubles.toList).toArray

      //if (verbose)
      //  System.out.println("\tall the rules\n" + ClassifierRule.toPrettyString(rules.toList))

      val simulationsCount = context(varSimulationCount)

      if (verbose) {
        val msg = "\tapplying subsumption on " + rulesShuffled.length + " unique rules " + rulesShuffled.map(_.name).mkString(",")
        System.out.println(msg)
        Log.log(Log.INFO, msg)
      }

      val rulesShuffledUsed = rulesShuffled.filter(r => r.applications > 0)
      val minPerIndicator: Array[Double] = (0 to microMinimize.length + microMaximize.length - 1).map(
        i ⇒ rulesShuffledUsed.map(
          r ⇒ r.min(i)
        ).min).toArray
      val maxPerIndicator: Array[Double] = (0 to microMinimize.length + microMaximize.length - 1).map(
        i ⇒ rulesShuffledUsed.map(
          r ⇒ r.max(i)
        ).max).toArray

      val epsilons = (minPerIndicator zip maxPerIndicator).map { case (min, max) ⇒ (max - min) / similarity.toDouble }

      if (verbose) {
        val msg = "\tusing epsilons on performance to define whether two rules can be merged or not:\n" +
          (microMinimize ++ microMaximize).zipWithIndex
          .map { case (indic, i) ⇒ "\t"+indic.simpleName + " [" + minPerIndicator(i) + ":" + maxPerIndicator(i) + "] => epsilon=" + epsilons(i) }
          .mkString(",\n")
        Log.log(Log.INFO,msg)
        System.out.println(msg)
      }

      val rulesUpdated = compareRules(epsilons, rulesShuffled.toList)

      if (verbose) {
        val msg = "\tmicro iteration " + iteration + "/" + maxIteration + " - rules after subsumption (capitalizing " +
          rulesUpdated.map(r ⇒ r.applications).sum + " micro simulations - over " + simulationsCount + " ran total):\n" +
          ClassifierRule.toPrettyString(rulesUpdated)
        System.out.println(msg)
        Log.log(Log.INFO, msg)

      }

      System.out.println("\tsubsumption reduced rules from " + rulesShuffled.length + " to " + rulesUpdated.length + " rules")

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