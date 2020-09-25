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
    parallelEval: FromContext[Int]
  ) = new DispatchEntities(parallelEval)

}

sealed class DispatchEntities(
  val parallelEval: FromContext[Int]
) extends Sampling with JavaLogger {

  override def inputs = List(
    varIterations,
    DecodeEntities.varEntities,
    varRules,
    varMin,
    varMax,
    varSimulationCount
  )
  override def prototypes = inputs //.map(_.toArray)

  override def apply(): FromContext[Iterator[Iterable[Variable[_]]]] = FromContext { ctxt ⇒
    import ctxt._

    // collect inputs
    
    val rules: Array[ClassifierRule] = context(varRules)
    val iteration: Int = context(varIterations)
    val entities: Array[Entity] = context(DecodeEntities.varEntities)
    val mins: Array[Double] = context(varMin)
    val maxs: Array[Double] = context(varMax)
    val simulationsCount: Int = context(varSimulationCount)
    
    val parallelCount: Int = parallelEval.from(context)

    //Log.log(Log.INFO, "dispatching ! Context: " + context)

    /*
    Log.log(Log.INFO, "dispatching " + rules.length + " rules for " + parallelCount + " parallel simulation")
    Log.log(Log.INFO, "entities: "+entities)
    Log.log(Log.INFO, "mins: "+mins)
    Log.log(Log.INFO, "maxs: "+maxs)

    Log.log(Log.INFO, "rules from context: "+context(varRules))
    //Log.log(Log.INFO, "rules from ctxt: "+ctxt(varRules))
    Log.log(Log.INFO, "rules from ctxt: "+varRules.from(ctxt.context))
  */

    //System.out.println("dispatching " + rules.length + " rules: " + rules.map(r ⇒ r.name).mkString(","))

    (1 to parallelCount).map(_ ⇒ List( 
          Variable(varIterations, iteration),
          Variable(DecodeEntities.varEntities, entities),
          Variable(varRules, rules),
          Variable(varMin, mins),
          Variable(varMax, maxs),
          Variable(varSimulationCount, simulationsCount)
        )
    ).toIterator
  }

}