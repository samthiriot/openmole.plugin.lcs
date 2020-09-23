name := "microlncs"

version := "0.1"

scalaVersion := "2.13.3"

enablePlugins(SbtOsgi)

OsgiKeys.exportPackage := Seq("org.openmole.plugin.method.microlcs.*")

OsgiKeys.importPackage := Seq("*;resolution:=optional")

OsgiKeys.privatePackage := Seq("*")

OsgiKeys.requireCapability := """osgi.ee;filter:="(&(osgi.ee=JavaSE)(version=1.8))""""

OsgiKeys.bundleActivator := Some("org.openmole.plugin.method.microlcs.Activator")


def openmoleVersion = "12.0-SNAPSHOT"

libraryDependencies += "org.openmole" %% "org-openmole-core-dsl" % openmoleVersion
libraryDependencies += "org.openmole" %% "org-openmole-core-pluginregistry" % openmoleVersion
