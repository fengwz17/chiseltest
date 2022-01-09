
package chiseltest.formal.backends.pono

import chiseltest.formal.backends._
import firrtl.backends.experimental.smt._

class PonoModelChecker(targetDir: os.Path) extends IsModelChecker {
  override val fileExtension = ".btor2"
  override val name:   String = "pono"
  override val prefix: String = "pono"

  override def check(sys: TransitionSystem, kMax: Int): ModelCheckResult = {
    // serialize the system to btor2
    val filename = sys.name + ".btor"
    // btromc isn't happy if we include output nodes, so we skip them during serialization
    val lines = Btor2Serializer.serialize(sys, skipOutput = true)
    os.write.over(targetDir / filename, lines.mkString("", "\n", "\n"))

    // execute model checker
    val kmaxOpt = if (kMax > 0) Seq("-k", kMax.toString) else Seq()
    // add options for pono
    val optionsPono = Seq("--witness") ++ Seq("-e") ++ Seq("bmc") 
    val cmd = Seq("./pono") ++ optionsPono ++ kmaxOpt ++ Seq(filename)
    val r = os.proc(cmd).call(cwd = targetDir, check = false)

    // write stdout to file for debugging
    val res = r.out.lines()
    os.write.over(targetDir / (filename + ".out"), res.mkString("", "\n", "\n"))
    println(r.exitCode)
    // check to see if we were successful
    assert(r.exitCode == 255 || r.exitCode == 0, 
        s"We expect pono to return 255 or 0, not ${r.exitCode}. Maybe there was an error.")
    val isSat = res.nonEmpty && res.head.trim.startsWith("sat")

    if (isSat) {
      val witness = PonoWitnessParser.read(res, 1).head
      ModelCheckFail(PonoModelChecker.convertWitness(sys, witness))
    } else {
      ModelCheckSuccess()
    }
  }
}

object PonoModelChecker {
  def convertWitness(sys: TransitionSystem, bw: PonoWitness): Witness = {
    val badNames = sys.signals.filter(_.lbl == IsBad).map(_.name).toIndexedSeq
    val failed = bw.failed.map(badNames)
    Witness(failed, bw.regInit, bw.memInit, bw.inputs)
  }
}
