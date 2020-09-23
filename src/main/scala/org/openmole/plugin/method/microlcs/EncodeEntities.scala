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
import org.openmole.core.workspace.NewFile

object EncodeEntities {

  // ugly.
  // but seriously, spending hours for manipulating generics is enough.
  def toArrayTyped(elems: Seq[Any]): Array[_] = elems(0) match {
    case _: Double  ⇒ elems.asInstanceOf[Seq[Double]].toArray
    case _: Integer ⇒ elems.asInstanceOf[Seq[Integer]].toArray
    case _: Boolean ⇒ elems.asInstanceOf[Seq[Boolean]].toArray
    case _: String  ⇒ elems.asInstanceOf[Seq[String]].toArray
    case _: Object  ⇒ elems.toArray
  }

  def apply[T](
    _characteristics: MicroCharacteristics,
    _actions:         Seq[MicroGenes.Gene[_]]
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("EncodeIndividuals") { (context, rng, _) ⇒

      // extract inputs from context
      val entities: Array[Entity] = context(DecodeEntities.varEntities)

      // debug:
      //System.out.println("encoding " + entities.length + " entities into " + (_characteristics.length + _actions.length) + " arrays...")

      // forge as many outputs as expected
      val outputsForCharacteristics: List[Variable[_]] =
        _characteristics
          .zipWithIndex
          .map {
            case (c, i) ⇒ // : Val[Array[Q]
              Variable.unsecure(
                c.prototype,
                toArrayTyped(
                  entities
                    .map(e ⇒ e.characteristics(i).value)
                )
              //.toArray(createGenericArray(c))
              )
          }
          .toList
      val outputsForActions: List[Variable[_]] =
        _actions
          .zipWithIndex
          .map {
            case (a, i) ⇒
              Variable.unsecure(
                a.prototype.toArray,
                toArrayTyped(
                  entities
                    .map(e ⇒ e.actions(i).value))
              )
          }
          .toList

      // cast the variables and return them as Arrays for each variable
      outputsForCharacteristics ++ outputsForActions

    } set (
      // we expect as inputs:
      // .. the list of entities
      (inputs, outputs) += DecodeEntities.varEntities,
      // ... the rules (well, just to forward them)

      (inputs, outputs) += varRules,
      (inputs, outputs) += varRulesApplied,

      // ... the current iterations
      (inputs, outputs) += varIterations,

      // we provide as outputs
      // ... the entities as arrays
      outputs ++= _characteristics.map(_.prototype),
      outputs ++= _actions.map(g ⇒ g.prototype.toArray).toSeq,

      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax,
      (inputs, outputs) += varSimulationCount

    //outputs += varRules,
    //outputs += varIterations

    )

  }
}
