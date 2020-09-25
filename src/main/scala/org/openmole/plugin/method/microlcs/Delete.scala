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

/**
 * ensures we only keep a given maximum of rules
 */
object Delete extends JavaLogger {

  def apply(
    maxrules:   Int,
    verbose:    Boolean = false
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("Delete") { (context, rng, _) ⇒


      // retrieve the inputs
      // ... the current iteration
      val iteration: Int = context(varIterations)

      // ... the rules used for the exploration
      val rules: Array[ClassifierRule] = context(varRules)

      val rulesTested: Array[ClassifierRule] = rules.filter(r ⇒ (r.applications > 0))
      val rulesNonTested: Array[ClassifierRule] = rules.filter(r ⇒ (r.applications == 0))

      // what is the expected proportion of rules we want to maintain ?
      val proportionNonTested: Double = 0.3
      val rulesNonTestedToKeep: Int = (rules.length * proportionNonTested).toInt.min(rulesNonTested.length)
      val rulesTestedToKeep: Int = maxrules - rulesNonTestedToKeep

      val rulesRankedPareto = HasMultiObjectivePerformance.detectParetoFronts(rulesTested)

      System.out.println("Iteration " + iteration + ": starting delete on " + rules.length + " rules...")

      //if (verbose)  
      //  System.out.println("\n" + HasMultiObjectivePerformance.paretoFrontsToPrettyString(rulesRankedPareto.take(1)))

      // select n parents; they will be taken from the first front, then next, then next, etc...
      val keptTested = HasMultiObjectivePerformance.selectParentsFromFronts(rulesTestedToKeep, rulesRankedPareto.toList)(rng).toArray
      val keptNonTested = rng().shuffle(rulesNonTested.toList).take(rulesNonTestedToKeep.max(maxrules - keptTested.length))
      val kept = keptTested ++ keptNonTested

      System.out.println("\tthere are " + rules.length + " rules, " +
        "we can only keep a max of " + maxrules + "; " +
        "we kept " + keptNonTested.length + " novel rules over " + kept.length + " rules")

      List(
        Variable(varRules, kept.toArray)
      )

    } set (
      // we expect as inputs:
      // ... the rules we used for each entity
      inputs += varRules,

      // we provide as outputs
      //outputs += DecodeEntities.varEntities,
      // ... the rules we updates with the novel information
      outputs += varRules,

      (inputs, outputs) += (varIterations),
      (inputs, outputs) += (DecodeEntities.varEntities),
      (inputs, outputs) += (varMin),
      (inputs, outputs) += (varMax),
      (inputs, outputs) += (varSimulationCount)

    )

  }
}