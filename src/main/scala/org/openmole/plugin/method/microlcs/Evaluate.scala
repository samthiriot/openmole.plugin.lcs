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

/**
 * Receives the results of simulations and the rules.
 * In charge of assessing how good those rules were.
 */
object Evaluate extends JavaLogger {

  def apply(
    microMinimize: Seq[Val[Double]],
    microMaximize: Seq[Val[Double]]
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("Evaluate") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the current iteration
      val iteration: Int = context(varIterations)

      // ... the rules used for the exploration
      val rulesUsed: Array[ClassifierRule] = context(varRulesApplied)
      val rulesUnused: Array[ClassifierRule] = context(varRulesApplied)

      // ... the indicators for each entity
      val microIndicatorsToMinimize: Seq[Array[Double]] = microMinimize.map(v ⇒ context(v.toArray))
      val microIndicatorsToMaximize: Seq[Array[Double]] = microMaximize.map(v ⇒ context(v.toArray))

      val entities = context(DecodeEntities.varEntities)

      //System.out.println("Iteration " + iteration + ": Evaluating the " + rulesUsed.length+" rules used during this simulation")

      // update each rule with the corresponding information
      //val rulesUpdated: Array[ClassifierRule] =
      rulesUsed.zipWithIndex
        .foreach {
          case (r, i) ⇒
            /*if (i <= 20) {
              System.out.println("on entity " + entities(i) + " rule " + r + " => " +
                (microIndicatorsToMinimize.map(vals ⇒ vals(i)) ++ microIndicatorsToMaximize.map(vals ⇒ -vals(i))).toList)
            }*/

            r.addPerformance(
              microIndicatorsToMinimize.map(vals ⇒ vals(i)) ++
                microIndicatorsToMaximize.map(vals ⇒ -vals(i))
            )
        }

      // System.out.println("Rules after evaluation: " + ClassifierRule.toPrettyString(rulesUsed.toList))
      List(
        Variable(varRules, (rulesUsed ++ rulesUnused).toSet.toArray),
        Variable(varIterations, iteration + 1)
      )

    } set (
      // we expect as inputs:
      // ... the performance of the entities over various indicators
      inputs ++= microMinimize.map(v ⇒ v.toArray),
      inputs ++= microMaximize.map(v ⇒ v.toArray),
      // ... the rules we used for each entity
      inputs += varRules,
      inputs += varRulesApplied,

      // we provide as outputs
      //outputs += DecodeEntities.varEntities,
      // ... the rules we updates with the novel information
      outputs += varRules,

      (inputs, outputs) += varIterations,
      (inputs, outputs) += DecodeEntities.varEntities,
      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax,
      (inputs, outputs) += varSimulationCount
    )

  }
}