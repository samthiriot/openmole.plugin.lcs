/*
 * Copyright (C) 2018 Samuel Thiriot
 *                    Romain Reuillon
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

import java.util.logging.{ Level, Logger }

import org.openmole.core.pluginregistry.PluginRegistry
import org.osgi.framework.{ BundleActivator, BundleContext }


class Activator extends BundleActivator {
  override def stop(context: BundleContext): Unit = PluginRegistry.unregister(this)

  override def start(context: BundleContext): Unit = {
    import org.openmole.core.highlight.HighLight._

    // force the level of communication
    // TODO remove this ?
    Logger.getLogger("org.openmole.plugin.method.microlcs").setLevel(Level.ALL)
    Logger.getLogger("org.openmole.tool.logger.JavaLogger").setLevel(Level.ALL)

    val highLight = Vector( // TODO
        // Sampling(objectName(MorrisSampling)),
        // Task(objectName(MorrisAggregation)),
        // Sampling(objectName(SaltelliSampling)),
        // TODO mimic instead  https://github.com/openmole/openmole/blob/9150960a4b6c5dd323ef82e2c193fc53ad2bd92f/openmole/plugins/org.openmole.plugin.method.sensitivity/src/main/scala/org/openmole/plugin/method/sensitivity/Activator.scala
        //TaskHighLight(objectName(MicroLCS)),
        //TaskHighLight(objectName(DiscoverPlansLCS)),
        WordHighLight("MicroLCS"),
        WordHighLight("DiscoverPlansLCS"),
        WordHighLight("MicroCharacteristic")
        //WordHighLight("DiscoverPlansLCS")
      )

    PluginRegistry.register(this, Vector(this.getClass.getPackage), highLight = highLight)

  }
}
