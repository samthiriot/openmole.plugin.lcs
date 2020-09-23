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

import org.openmole.core.context.{ Context, Val, Variable }
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.tool.logger.JavaLogger

/**
 * This simple utilitary task receives several variables containing arrays of values
 * which corresponds to the characteristics of several entities delivered independantly;
 * it converts these arrays into a unique array of entities.
 */
object DecodeEntities extends JavaLogger {

  // the value which will contain the list of the entities in the model
  val varEntities = Val[Array[Entity]]("entities", namespace = namespaceMicroLCS)

  val varMin = Val[Array[Double]]("characteristic_min", namespace = namespaceMicroLCS)
  val varMax = Val[Array[Double]]("characteristic_max", namespace = namespaceMicroLCS)

  def toIndividuals(context: Context, characteristics: Seq[Val[Array[_]]], actions: Seq[Val[_]], acc: Seq[Entity]) = {

    val truc: Val[_] = null

    val noMoreCharacteristic: Boolean = characteristics.exists(l ⇒ context(l).isEmpty)

    //val currentCharacteristics: List[Val[_]] = characteristics.map(l ⇒ context(l).head).toList

  }

  def apply[T](
    _characteristics: MicroCharacteristics,
    _actions:         Seq[MicroGenes.Gene[_]]
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("DecodeIndividuals") { (context, rng, _) ⇒

      // check input parameters: all the arrays for characteristics should be of the right size
      // ... build the list of the length of each list passed for characteristics, and keep only distinct values
      val countsEntities: List[Int] = _characteristics.map(v ⇒ context(v.prototype).length).toList.distinct
      // ... there should be only
      if (countsEntities.length > 1) {
        throw new IllegalArgumentException("the characteristics arrays should have the same length")
      }

      val countEntities: Int = countsEntities.head

      Log.log(Log.INFO, "There are " + countEntities + " entities for this study having each " + _characteristics.size + " characteristics and " + _actions.size + " parameters")

      val _characteristicsValues: Seq[Array[_]] = _characteristics.map(v ⇒ context(v.prototype))

      val entities: Array[Entity] = Array.tabulate(countEntities)(
        idx ⇒
          Entity(
            id = idx,
            characteristics = _characteristics.zipWithIndex.map { case (c, i) ⇒ Variable.unsecure(Val(c.prototype.name)(c.prototype.`type`), _characteristicsValues(i)(idx)) }.toArray,
            actions = _actions.map(a ⇒ Variable.unsecure(a.prototype, null /*a.makeRandomValue(context)(rng, newFile, fileService)*/ )).toArray
          )
      )

      val min: Array[Double] = _characteristicsValues.map {
        case dd: Array[Double] ⇒ dd.min
        case ii: Array[Int]    ⇒ ii.min.toDouble
        case _                 ⇒ Double.NaN
      }.toArray

      val max: Array[Double] = _characteristicsValues.map {
        case dd: Array[Double] ⇒ dd.max
        case ii: Array[Int]    ⇒ ii.max.toDouble
        case _                 ⇒ Double.NaN
      }.toArray

      // cast the variables and return them as Arrays for each variable
      List(
        // here are the entities we just decoded
        Variable(DecodeEntities.varEntities, entities),

        // at the beginning there is no rule
        Variable(varRules, Array[ClassifierRule]()),

        // at the beginning it is the first iteration
        Variable(varIterations, 0),

        // at this point we ran nothing
        Variable(varSimulationCount, 0),

        Variable(varMin, min),
        Variable(varMax, max)
      )

    } set (
      // we expect as inputs:
      // ... the characteristics of the individuals we should receive
      inputs ++= _characteristics.map(_.prototype),
      // ... the list of variables for the outputs
      //inputs ++= _actions,
      // we provide as outputs
      outputs += (
        // ... the entities we decoded
        DecodeEntities.varEntities,
        // ... the rules (will be empty out of here !)
        varRules,
        // ... the count of iteration
        varIterations,
        // the count of simulations
        varSimulationCount,
        // ... the min and max values for each characteristic
        varMin, varMax
      )
    )

  }
}
