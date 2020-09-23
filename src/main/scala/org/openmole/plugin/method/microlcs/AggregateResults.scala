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
object AggregateResults extends JavaLogger {

  def apply()(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("AggregateResults") { (context, rng, _) ⇒

      // retrieve the inputs
      // ... the current iteration
      val iterations: Array[Int] = context(varIterations.toArray)
      if (iterations.distinct.length > 1) {
        throw new RuntimeException("error during execution: we ended with different and incompatible values for iterations " + iterations.toList)
      }
      val iteration: Int = iterations(0)

      // the rules
      // ... the rules
      val rules: Array[Array[ClassifierRule]] = context(varRules.toArray)
      val entities: Array[Array[Entity]] = context(DecodeEntities.varEntities.toArray)
      val mins: Array[Array[Double]] = context(DecodeEntities.varMin.toArray)
      val maxs: Array[Array[Double]] = context(DecodeEntities.varMax.toArray)

      val minsFlatten: Array[Double] = mins.transpose.map(vv ⇒ vv.min)
      val maxsFlatten: Array[Double] = maxs.transpose.map(vv ⇒ vv.max)

      val simulationsCounts: Array[Int] = context(varSimulationCount.toArray)
      val simulationsCount = simulationsCounts(0) + simulationsCounts.length

      List(
        Variable(varIterations, iteration),
        Variable(varRules, rules.flatten),
        Variable(DecodeEntities.varEntities, entities(0)),
        Variable(DecodeEntities.varMin, minsFlatten),
        Variable(DecodeEntities.varMax, maxsFlatten),
        Variable(varSimulationCount, simulationsCount)
      )

    } set (

      inputs += (varIterations.toArray, varRules.toArray, DecodeEntities.varEntities.toArray, DecodeEntities.varMin.toArray, DecodeEntities.varMax.toArray, varSimulationCount.toArray),
      outputs += (varIterations, varRules, DecodeEntities.varEntities, DecodeEntities.varMin, DecodeEntities.varMax, varSimulationCount)
    )

  }
}