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

import org.openmole.core.context.Context
import org.openmole.core.fileservice.FileService
import org.openmole.core.workspace.NewFile
import org.openmole.tool.logger.JavaLogger
import org.openmole.tool.random.RandomProvider

/**
 * A Macro gene refers to a gene coding an intervention
 * at the scale of the entire model.
 * It is made of several classifier rules and has a performance
 * evaluated over one or more macro indicators
 */
case class MacroGene(
  id:                 Int,
  name:               String,
  rules:              Array[ClassifierRule],
  override var means: Seq[Double]           = Seq(),
  override var min:   Seq[Double]           = Seq(),
  override var max:   Seq[Double]           = Seq()
) extends HasMultiObjectivePerformance {

  override def toString: String = "Plan " + name + ": " +
    performanceToString() +
    "\n\t     " + rules.map(_.toStringWithoutPerformance).mkString("\n\telse ")
}

object MacroGene extends JavaLogger {

  var lastId: Int = 0

  def nameForId(id: Int) = Integer.toString(id, 36).toUpperCase

  def nextId(existing: Array[MacroGene]): Int = existing.map(_.id).max + 1

  def apply(id: Int, rules: Array[ClassifierRule]): MacroGene = {

    lastId = if (id > lastId) id else lastId

    MacroGene(
      id,
      nameForId(id),
      rules,
      Seq()
    )
  }

  def toPrettyString(g: Iterable[MacroGene]): String = g.map(_.toString).mkString(",\n")

  def mutate(
    p:            MacroGene,
    microActions: Seq[MicroGenes.Gene[_]],
    mins:         Array[Double],
    maxs:         Array[Double],
    maxId:        Int,
    proportions:  Seq[Double],
    context:      Context
  )(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): MacroGene = {

    // TODO mutate !

    // don't mutate the last one ???

    // we can mutate:
    // * only conditions, not actions (why ?)
    // * only the first rules, not the last one
    // *

    val rand = rng()

    // select the rule we will mutate
    val idxRule: Int = rand.nextInt(p.rules.length - 1)

    val rulesMutated = p.rules.slice(0, idxRule) ++
      List(
        ClassifierRule.mutateConditionOrProportion(
          p.rules(idxRule),
          mins, maxs, maxId,
          proportions,
          context)) ++
        p.rules.slice(idxRule + 1, p.rules.length)

    lastId = lastId + 1
    val updated = p.copy(
      id = lastId,
      name = nameForId(lastId),
      means = Seq(), min = Seq(), max = Seq(),
      rules = rulesMutated
    )

    Log.log(Log.FINE, "mutated rule " + idxRule + ":\n\tfrom: " + p + "\n\t  to: " + updated)

    updated
  }

}
