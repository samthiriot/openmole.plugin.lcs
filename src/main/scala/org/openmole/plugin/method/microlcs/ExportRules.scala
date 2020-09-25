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
 * Changes the list of rules into basic types
 * which can be stored as CSV
 */
object ExportRules extends JavaLogger {

  val varId = Val[Array[String]]("id", namespace = namespaceMicroLCS)
  val varCount = Val[Array[Int]]("count", namespace = namespaceMicroLCS)
  val varIterationRule = Val[Array[Int]]("iteration", namespace = namespaceMicroLCS)
  /*
  def toTypedArray[T: ClassTag](l: List[T]): Array[_] = l match {
    case li: List[Int]     ⇒ li.toArray[Int]
    case ld: List[Double]  ⇒ ld.toArray[Double]
    case lb: List[Boolean] ⇒ lb.toArray[Boolean]
    case ls: List[String]  ⇒ ls.toArray[String]
    case lo: List[Object]  ⇒ lo.toArray[Object]
  }
  */

  def apply(
    microCharacteristics: MicroCharacteristics,
    microActions:         Seq[MicroGenes.Gene[_]],
    microMinimize:        Seq[Val[Double]],
    microMaximize:        Seq[Val[Double]]
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, fileService: FileService) = {

    ClosureTask("ExportRules") { (context, rng, _) ⇒

      val iteration: Int = context(varIterations)
      val rules: Array[ClassifierRule] = context(varRules)

      // remove the rules with no experience
      val rulesFiltered: Array[ClassifierRule] = rules.filter(r ⇒ (r.applications > 0))

      System.out.println("Exportation: over the "+rules.length+" we kept "+rulesFiltered.length+" rules")
      //Log.log(Log.FINER, "preparing " + rulesFiltered.length + " rules for exportation")

      List(
        Variable(varId, rulesFiltered.map(r ⇒ r.name))
      ) ++ microCharacteristics.zipWithIndex.map {
          case (c, i) ⇒ Variable(
            Val[Array[String]](c.prototype.name, namespace = namespaceMicroLCS),
            rulesFiltered.map(r ⇒ r.conditions(i).toString))
        } ++ microActions.zipWithIndex.map {
          case (a, i) ⇒ Variable.unsecure(
            a.prototype.toArray,
            EncodeEntities.toArrayTyped(rulesFiltered.toList.map(r ⇒ r.actions(i).value).toList))
        } ++ (microMinimize ++ microMaximize).zipWithIndex.map {
          case (t, i) ⇒ Variable(
            t.toArray,
            rulesFiltered.map(r ⇒ r.performanceAggregated(i))
          )
        } ++ List(
          Variable(varCount, rulesFiltered.map(r ⇒ r.applications)),
          Variable(varIterationRule, rulesFiltered.map(_ ⇒ iteration))
        )

    } set (
      // we expect as inputs:
      // ... the rules we used for each entity
      inputs += varRules,
      inputs += varIterations,

      // we provide as outputs
      //outputs += DecodeEntities.varEntities,
      // ... the rules we updates with the novel information
      outputs += varId,
      outputs += varCount,
      outputs += varIterationRule,
      outputs += varIterations,
      outputs ++= microCharacteristics.map(c ⇒ Val[Array[String]](c.prototype.name, namespace = namespaceMicroLCS)),
      outputs ++= microActions.map(a ⇒ a.prototype.toArray),
      outputs ++= (microMinimize ++ microMaximize).map(t ⇒ t.toArray)

    )

  }
}