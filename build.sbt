name := "scodec-protobuf"

version := "1.0.0-SNAPSHOT"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "senexus"                at "https://nexus.simpleenergy.com/content/repositories/releases",
  "senexus snapshot"       at "https://nexus.simpleenergy.com/content/repositories/snapshots",
  Resolver.url("senexus ivy", url("https://nexus.simpleenergy.com/content/repositories/ivy-releases"))(Resolver.ivyStylePatterns)
)

libraryDependencies ++= Seq(
  "org.typelevel"   %% "scodec-core" % "1.1.0-SE-SNAPSHOT",
  "org.specs2"      %% "specs2"      % "2.3.10-scalaz-7.1.0-M6" % "test",
  "org.scalacheck"  %% "scalacheck"  % "1.11.3" % "test"
)

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
