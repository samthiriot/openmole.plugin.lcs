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
 * Takes a list of rules, the entities and existing macro plans
 * as inputs.
 * Attempts to combine rules into macro plans and exports them
 * so they can be evaluated.
 */
object SamplePlans extends JavaLogger {

  def apply() = new SamplePlans()

}

sealed class SamplePlans() extends Sampling with JavaLogger {

  override def inputs = List(
    varIterations,
    DecodeEntities.varEntities,
    varPlans,
    varRules,
    DecodeEntities.varMin, DecodeEntities.varMax,
    varPlansBefore,
    varSimulationCount
  )

  override def prototypes = List(
    varIterations,
    DecodeEntities.varEntities,
    varRules,
    varPlanSimulated,
    DecodeEntities.varMin, DecodeEntities.varMax,
    varPlansBefore,
    varSimulationCount
  )

  override def apply(): FromContext[Iterator[Iterable[Variable[_]]]] = FromContext { ctxt ⇒
    import ctxt._

    // collect inputs
    val iteration: Int = context(varIterations)
    val entities: Array[Entity] = context(DecodeEntities.varEntities)
    //val rules: Array[ClassifierRule] = context(varRules)
    val plans: Array[MacroGene] = context(varPlans)

    // TODO a virer ?
    val mins: Array[Double] = context(DecodeEntities.varMin)
    val maxs: Array[Double] = context(DecodeEntities.varMax)

    val allRules: Array[ClassifierRule] = context(varRules)

    // TODO to be removed
    val previousPlans: Array[MacroGene] = context(varPlansBefore)

    val iterationsCount = context(varSimulationCount)

    Log.log(Log.FINER, "Iteration " + iteration + ": dispatching the " + plans.length + " plans for evaluation")

    List(
      // just duplicate the same information for every run
      plans.map(_ ⇒ Variable(varIterations, iteration)).toList,
      plans.map(_ ⇒ Variable(DecodeEntities.varEntities, entities)).toList,
      // dispatch the plans
      plans.map(p ⇒ Variable(varRules, p.rules)).toList,
      plans.map(p ⇒ Variable(varPlanSimulated, p)).toList,
      // TODO virer ?
      plans.map(_ ⇒ Variable(DecodeEntities.varMin, mins)).toList,
      plans.map(_ ⇒ Variable(DecodeEntities.varMax, maxs)).toList,
      plans.map(_ ⇒ Variable(varPlansBefore, previousPlans)).toList,
      plans.map(_ ⇒ Variable(varSimulationCount, iterationsCount)).toList
    ).toIterator
  }

}