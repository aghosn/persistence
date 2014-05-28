package scala.reflect.persistence.sbt /* TODO: check for proper package */

import sbt._
import Keys._


object AstcPlugin extends Plugin {
  override lazy val projectSettings = Seq(packageAstTask, beforeCompileTask) ++ usePluginSettings ++ newCompile

  /* TODO: later, when the SBT plugin will be published, we will required to add it as 
   * addCompilerPlugin("..." %% "..." % _).
   * For now, let's just add it as a path option. */
  /* Get the path to the plugin jar (should be generated prior to the test) */
  val astcJar = new File("../../plugin/target/scala-2.11/plugin-assembly-0.1.0-SNAPSHOT.jar")
  lazy val usePluginSettings = Seq( // TODO: addCompilerPlugin instead, command already made for that purpose
      scalacOptions ++= Seq("-Xplugin:" + astcJar.getAbsolutePath)
  )

  /* TODO: check what we need as a Manifest(if relevant) */
  val packageAst = TaskKey[File]("package-ast", "Produce an artifact containing compressed Scala ASTs.")
  val packageAstTask = packageAst := {
    (compile in Compile).value     /* First let's compile everything */
    val generalPath = new File((classDirectory in Compile).value.getParent).getAbsolutePath
    val astsPath = generalPath + "/asts/"
    val outputJar = new File(generalPath + "/" + name.value + "_" + version.value + "-asts.jar")
    val astsSources = findFiles(new File(astsPath))
    val log = streams.value.log
    val manifest = new java.util.jar.Manifest()
    Package.makeJar(astsSources.map(f => (f, f.getAbsolutePath.replace(astsPath, ""))), outputJar, manifest, log)
    outputJar
  }
  def findFiles(root: File): List[File] = root match {
      case _ if root.isDirectory => root.listFiles.toList.flatMap(f => findFiles(f))
      case _ => root :: Nil
  }
  
  /* Save the previously generated .ast files */
  val beforeCompile = TaskKey[Unit]("before-compile")
  val beforeCompileTask = beforeCompile := {
    val generalPath = new File((classDirectory in Compile).value.getParent).getAbsolutePath
    (new File(generalPath + "/asts/")).renameTo(new File(generalPath + "/asts.bak"))
  }
  /* If the compilation was successful, then we remove the old .asts. Otherwise we restore them. */
  val newCompile = compile in Compile := ({
    beforeCompile.value /* Save the .asts */
    val generalPath = new File((classDirectory in Compile).value.getParent).getAbsolutePath
    var res = (compile in Compile).result.value match {
    case Inc(inc: Incomplete) =>
    (new File(generalPath + "/asts.bak/")).renameTo(new File(generalPath + "/asts"))
      throw inc
    case Value(analysis) =>
    if (new File(generalPath + "asts/").exists) (new File(generalPath + "/asts.bak/")).delete
    else (new File(generalPath + "/asts.bak/")).renameTo(new File(generalPath + "/asts"))
      analysis
  }
  res
  })

}