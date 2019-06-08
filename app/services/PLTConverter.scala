package services

object PLTConverter {
  private[this] case class TextInfo(section: String, count: Int, textVarCount: Int, lineNum: Int, text: Seq[String])

  /**
    * PLTファイルを整形する
    * @param PLTファイルの各行を要素としたSeq
    * @return 整形結果のPLTファイルの内容
    */
  private[this] def convert(fileContents: Seq[String]): String = {

    /**
      * 除外対象の行を判定する
      * @param PLTファイルの各行
      * @return 除外すべき行か否か
      */
    def isRemove(txt: String): Boolean = {
      val splitNum = txt.split(" ").length
      val lastCharIsSpaceChar = splitNum > 1 && splitNum < 21
      val returnChar = splitNum == 1 && (txt+"a") == "a"
      returnChar || lastCharIsSpaceChar
    }

    val textInfo = fileContents.foldLeft(TextInfo("", 0, 0, 0, Seq())) {(acc, line) =>
      val s = line.trim
      s match {
        case "[TEXT_VAR]" => TextInfo("TEXT_VAR", acc.count + 1, acc.textVarCount, acc.lineNum, acc.text :+ s)
        case "[ENVIRO]" => TextInfo("", acc.count + 1, acc.textVarCount, acc.lineNum, acc.text :+ ("\n" + s))
        case s if acc.section == "TEXT_VAR" => {
          if (acc.lineNum == 0) TextInfo(acc.section, acc.count + 1, acc.textVarCount, acc.count, acc.text :+ s)
          else if (!isRemove(s)) TextInfo(acc.section, acc.count + 1, acc.textVarCount + 1, acc.lineNum, acc.text :+ s) else acc
        }
        case _ => TextInfo(acc.section, acc.count + 1, acc.textVarCount, acc.lineNum, acc.text :+ s)
      }
    }

    val result = textInfo.text.zipWithIndex.map{case (x, i) => if (i == textInfo.lineNum) textInfo.textVarCount.toString else x}.mkString("\n")
    result
  }

  def apply(fileName: String): String = {
    val file = scala.io.Source.fromFile(fileName, "Shift_JIS")
    convert(file.getLines.toList)
  }
}