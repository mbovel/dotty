// Add personal SBT plugins for IDEs, etc to `local-plugins.sbt`
//
// e.g. addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.1.0")

// some plugins haven't moved to scala-xml 2.x yet
libraryDependencySchemes +=
  "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.18.1")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.21")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.2.1")

addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.4.7")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % "1.1.2")

addSbtPlugin("ch.epfl.scala" % "sbt-tasty-mima" % "1.0.0")

addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.10.0")

resolvers += "Develocity Artifactory" at "https://repo.grdev.net/artifactory/public/"
addSbtPlugin("com.gradle" % "sbt-develocity" % "1.2-rc-2")

addSbtPlugin("com.github.sbt" % "sbt-jdi-tools" % "1.2.0")
