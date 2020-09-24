organization := "ch.resear.samthiriot.openmole"
name := "microlncs"

version := "0.1"

scalaVersion := "2.13.3"

enablePlugins(SbtOsgi)

osgiSettings

OsgiKeys.importPackage := Seq("*;resolution:=optional")

//OsgiKeys.privatePackage := Seq("*")
OsgiKeys.privatePackage := Seq("!scala.*")

OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""

OsgiKeys.exportPackage := Seq("org.openmole.plugin.method.microlcs.*")
OsgiKeys.bundleActivator := Some("org.openmole.plugin.method.microlcs.Activator")


libraryDependencies += "org.osgi" % "org.osgi.framework" % "1.9.0" % Provided


def openmoleVersion = "12.0-SNAPSHOT"

libraryDependencies += "org.openmole" %% "org-openmole-core-context" % openmoleVersion % Provided
libraryDependencies += "org.openmole" %% "org-openmole-core-dsl" % openmoleVersion % Provided
libraryDependencies += "org.openmole" %% "org-openmole-core-expansion" % openmoleVersion % Provided
libraryDependencies += "org.openmole" %% "org-openmole-core-fileservice" % openmoleVersion % Provided
libraryDependencies += "org.openmole" %% "org-openmole-core-pluginregistry" % openmoleVersion % Provided
libraryDependencies += "org.openmole" %% "org-openmole-core-workflow" % openmoleVersion % Provided
libraryDependencies += "org.openmole" %% "org-openmole-core-workspace" % openmoleVersion % Provided

libraryDependencies += "org.openmole" %% "org-openmole-tool-logger" % openmoleVersion % Provided
libraryDependencies += "org.openmole" %% "org-openmole-tool-random" % openmoleVersion % Provided

// TODO remove once tests are done 
libraryDependencies += "org.openmole" %% "org-openmole-plugin-hook-display" % openmoleVersion % Provided


OsgiKeys.embeddedJars := Seq()
OsgiKeys.explodedJars := Seq()

