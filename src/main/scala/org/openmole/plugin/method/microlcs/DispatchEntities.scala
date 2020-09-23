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
import org.openmole.core.expansion.FromContext
import org.openmole.core.workflow.sampling.Sampling
import org.openmole.tool.logger.JavaLogger

/**
 * Takes the inputs necessary for matching, and
 * changes them into n packets which enable parallel simulation
 */
object DispatchEntities extends JavaLogger {

  def apply(
    parallelEval: Int
  ) = new DispatchEntities(parallelEval)

}

sealed class DispatchEntities(
  val parallelEval: Int
) extends Sampling with JavaLogger {

  override def inputs = List(
    varIterations,
    DecodeEntities.varEntities,
    varRules,
    DecodeEntities.varMin,
    DecodeEntities.varMax,
    varSimulationCount
  )
  override def prototypes = inputs

  override def apply(): FromContext[Iterator[Iterable[Variable[_]]]] = FromContext { ctxt ⇒
    import ctxt._

    // collect inputs
    val iteration: Int = context(varIterations)
    val entities: Array[Entity] = context(DecodeEntities.varEntities)
    val rules: Array[ClassifierRule] = context(varRules)
    val mins: Array[Double] = context(DecodeEntities.varMin)
    val maxs: Array[Double] = context(DecodeEntities.varMax)
    val simulationsCount: Int = context(varSimulationCount)

    Log.log(Log.INFO, "dispatching " + rules.length + " for " + parallelEval + " parallel simulation")
    //System.out.println("dispatching " + rules.length + " rules: " + rules.map(r ⇒ r.name).mkString(","))

    List(
      (0 to parallelEval).map(_ ⇒ Variable(varIterations, iteration)),
      (0 to parallelEval).map(_ ⇒ Variable(DecodeEntities.varEntities, entities)),
      (0 to parallelEval).map(_ ⇒ Variable(varRules, rules)),
      (0 to parallelEval).map(_ ⇒ Variable(DecodeEntities.varMin, mins)),
      (0 to parallelEval).map(_ ⇒ Variable(DecodeEntities.varMax, maxs)),
      (0 to parallelEval).map(_ ⇒ Variable(varSimulationCount, simulationsCount))
    ).toIterator
  }

}