ThisBuild / scalaVersion := "3.2.2"

resolvers ++= Seq (
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/releases",
  "Maven releases" at "https://repo1.maven.org/maven2/")

val OrToolsVersion = "9.6.2534" // NOTE - 8.2.9004 tried upgrading to 9.0.9048, but the upgrade fails to upload.
val JnaPlatform = "5.7.0" // NOTE - harmonises with or-tools, which packs dylibs into darwin, not darwin-x86-64
val ScalaTestVersion = "3.2.11"
val ProtobufVersion = "3.17.3"
val H3Version = "3.7.2"
val JsonIterVersion = "2.13.10"
val Matplotlib4jVersion = "0.5.0"
val VulcanVersion = "1.7.1"
val ApacheMathVersion = "3.6.1"
val ZioHttpVersion = "2.0.0-RC5"
val ZioPreludeVersion = "1.0.0-RC18"

fork / run := true

lazy val sim =
  project
    .in (file("."))
    .settings (
      libraryDependencies ++= Seq (
        "com.uber" % "h3" % H3Version,
        "com.github.sh0nk" % "matplotlib4j" % Matplotlib4jVersion,
        "com.google.ortools" % "ortools-darwin-x86-64" % OrToolsVersion,
        "com.google.ortools" % "ortools-java" % OrToolsVersion,
        "com.google.protobuf" % "protobuf-java" % ProtobufVersion,
        "io.d11" %% "zhttp" % ZioHttpVersion,
        "org.apache.commons" % "commons-math3" % ApacheMathVersion,
        "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
        "net.java.dev.jna" % "jna-platform" % JnaPlatform,
        "org.scala-lang" %% "scala3-library" % "3.2.2",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % JsonIterVersion,
        "dev.zio" %% "zio-prelude" % ZioPreludeVersion
      ))
