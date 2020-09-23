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

import org.openmole.core.context.{ Context, Variable }
import org.openmole.core.fileservice.FileService
import org.openmole.core.workflow.builder.DefinitionScope
import org.openmole.core.workflow.dsl._
import org.openmole.core.workflow.task.ClosureTask
import org.openmole.core.workspace.NewFile
import org.openmole.tool.logger.JavaLogger
import org.openmole.tool.random.RandomProvider

import scala.annotation.tailrec

object Matching extends JavaLogger {

  def covering(entity: Entity, _actions: Seq[MicroGenes.Gene[_]], context: Context)(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule = {
    ClassifierRule(entity, _actions, context)
  }

  def applyOneRuleDeterministic(matching: List[ClassifierRule])(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): ClassifierRule =
    if (matching.length == 1) {
      matching(0)
    }
    else {
      val r :: tail = matching
      if (r.proportion == 1.0 || rng().nextDouble() <= r.proportion) {
        r
      }
      else
        applyOneRuleDeterministic(tail)
    }

  @tailrec
  def matchOrCoverEntities(
    rulesAvailable:   Array[ClassifierRule],
    entitiesToCover:  List[Entity],
    _actions:         Seq[MicroGenes.Gene[_]],
    context:          Context,
    deterministic:    Boolean,
    entitiesCovered:  List[Entity],
    rulesForEntities: List[ClassifierRule]
  )(implicit rng: RandomProvider, newFile: NewFile, fileService: FileService): Array[ClassifierRule] = entitiesToCover match {
    case Nil ⇒ rulesForEntities.toArray
    case e :: tail ⇒
      val matching: Array[ClassifierRule] = rulesAvailable.filter(r ⇒ r.matches(e))
      val (r, createdRule) = {
        if (matching.length == 1) {
          // only one rule is matching; let's use this one
          (matching(0), false)
        }
        else if (matching.isEmpty) {
          // no rule is matching; let's run the covering mechanism
          (covering(e, _actions, context), true)
        }
        else if (deterministic) {
          // there are several rules, which have to be processed deterministically;
          // let's pick the first one matching
          // note we take the proportion into account
          (applyOneRuleDeterministic(matching.toList), false)
        }
        else {
          // there are several rules matching; and we are not deterministic.
          // let's pick up a random one
          // note we ignore the proportion here

          // uniform selection here:
          // (matching(rng().nextInt(matching.length)), false)

          // bias the selection:
          // we prefer to select rules which were not tested a lot
          val weights = matching.map(r ⇒ 100 - math.log(1 + r.applications))
          (weightedWheel(matching, weights)(rng), false)
        }
      }
      val rulesAvailableUpdated = if (createdRule) { rulesAvailable :+ r } else rulesAvailable
      matchOrCoverEntities(
        rulesAvailableUpdated,
        tail,
        _actions,
        context,
        deterministic,
        entitiesCovered ++ List(e),
        rulesForEntities ++ List(r)
      )

  }
  def apply(
    _actions:      Seq[MicroGenes.Gene[_]],
    deterministic: Boolean
  )(implicit name: sourcecode.Name, definitionScope: DefinitionScope, newFile: NewFile, fileService: FileService) = {

    ClosureTask("Matching") { (context, rng, _) ⇒

      val iteration: Int = context(varIterations)
      val entities: Array[Entity] = context(DecodeEntities.varEntities)
      val rules: Array[ClassifierRule] = context(varRules)

      val rulesShuffled: Array[ClassifierRule] = deterministic match {
        case true  ⇒ rules
        case false ⇒ rng().shuffle(rules.toList).toArray
      }

      // debug:
      /*System.out.println(
        "Iteration " + iteration +
          " matching on " + entities.length + " entities " +
          "based on " + rules.length + " rules: " + rulesShuffled.map(_.name).mkString(","))
      */

      // create the set of actions to be used
      val rulesActionSet: Array[ClassifierRule] =
        matchOrCoverEntities(rulesShuffled, entities.toList, _actions, context, deterministic, List(), List())(rng, newFile, fileService)

          //entities.map { e ⇒ matchOrCoverIndividual(rulesShuffled, e, _actions, context, deterministic)(rng, newFile, fileService) }
          .toArray
      //System.out.println("Here are the rules: " + ClassifierRule.toPrettyString(rulesActionSet.toList))

      // apply the rules on entities
      val entitiesUpdated: Array[Entity] =
        entities.zipWithIndex.map { case (e, i) ⇒ rulesActionSet(i).actUpon(e) }
          .toArray

      List(
        Variable(DecodeEntities.varEntities, entitiesUpdated),
        Variable(varRulesApplied, rulesActionSet)
      )

    } set (
      // we expect as inputs:
      // ... the entities to match
      inputs += DecodeEntities.varEntities,
      // ... the list of rules
      inputs += varRules,
      // .. the current iteration
      inputs += varIterations,

      // we provide as outputs
      outputs += DecodeEntities.varEntities,
      // ... the entities we decoded
      outputs += varRulesApplied,
      outputs += varRules,
      // ... the current iteration
      outputs += varIterations,

      (inputs, outputs) += varSimulationCount,
      (inputs, outputs) += DecodeEntities.varMin,
      (inputs, outputs) += DecodeEntities.varMax
    )

  }

}
