package services

import scala.xml.{Node, XML}

object POListPrinter {
  /**
    *
    * @param fbLocalId この変数が結線されているFBのlocalId
    * @param terminalName この変数が結線されている端子の名称
    */
  case class ConnectionPointIn(fbLocalId: String, terminalName: String)

  /**
    * Pou内で何らかの出力に使用されている変数を表す
    * @param name 変数の名前
    * @param connection 接続先
    */
  case class OutVar(name: String, connection: ConnectionPointIn)

  /**
    * FBの情報を格納する
    * @param localId FBのlocalId
    * @param name FB名称
    * @param typeName FB種別
    * @param inputVars FBの入力端子の結線情報
    * @param outputVars FBの出力端子の結線情報
    * @param inoutVars FBの入出力端子の結線情報
    */
  case class FunctionBlock(localId: String, name: String, typeName: String, comment: String, inputVars: Seq[Terminal], outputVars: Seq[Terminal], inoutVars: Seq[Terminal]) {
    /**
      * PONo/SPONoを持つFBか判定する
      * @return
      */
    def hasPO: Boolean = inputVars.exists(t => t.name == "PONO" || t.name == "SPONO")

    def poNo: String = inputVars.filter(t => t.name == "PONO" || t.name == "SPONO") match {
      case x::_ => x.varName
      case Nil => "0"
    }

    def epoNo: String = inputVars.filter(t => t.name == "EPONO" || t.name == "SEPONO") match {
      case x::_ => x.varName
      case Nil => "0"
    }

    /**
      * POかSPOかを返す
      * @return
      */
    def poType: String = inputVars.filter(t => t.name == "PONO" || t.name == "SPONO") match {
      case x::_ => x.name
      case Nil => ""
    }
  }

  /**
    * FBの端子情報を格納する
    * @param name FBの端子名称
    * @param refLocalId 端子に結線されている変数のlocalId
    */
  case class Terminal(name: String, refLocalId: String, varName: String, varComment: String)



  /**
    * pouから変数名称と変数コメントを対応付けたMapを取得する
    * @param pou Node
    * @return 変数と変数コメントのMap
    */
  def getVarNameMap(pou: Node): Map[String, String] = {
    val vars = pou \\ "variable"
    vars.map(v => {
      val varName = (v \ "@name").toString
      val varComment = (v \ "documentation" \ "html" \ "p").text
      varName -> varComment
    }).toMap
  }

  /**
    * pou内で何らかの入力に使用されている変数のlocalIdと変数名称のMapを取得する
    * @param pou Node
    * @return pou内で何らかの入力に使用されている変数のlocalIdと変数名称のMap
    */
  def getInVarMap(pou: Node): Map[String, String] = {
    val inVars = pou \\ "inVariable"
    inVars.map(v => {
      val localId = (v \ "@localId").toString
      val varName = (v \ "expression").text
      localId -> varName
    }).toMap
  }

  /**
    * pou内で何らかの出力に使用されている変数のlocalIdと出力先の情報のMapを取得する
    * @param pou Node
    * @return pou内で何らかの出力に使用されている変数のlocalIdと出力先の情報のMap
    */
  def getOutVarMap(pou: Node): Map[ConnectionPointIn, String] = {
    val outVars = pou \\ "outVariable"
    outVars.map(v => {
      val localId = (v \ "@localId").toString
      val connection = v \ "connectionPointIn" \ "connection"
      val formalParameter = (connection \ "@formalParameter").toString
      val refLocalId = (connection \ "@refLocalId").toString
      val varName = (v \ "expression").text
      ConnectionPointIn(refLocalId, formalParameter) -> varName
    }).toMap
  }

  /**
    * POU内に含まれるFBの情報を取得する
    * @param pou Node
    * @return POU内に含まれるFBの情報のSeq
    */
  def getFunctionBlock(pou: Node): Map[String, FunctionBlock] = {
    def getVars(varType: String, fb: Node): Seq[Terminal] = {
      val variables = fb \ varType \\ "variable"
      variables.map(v => {
        val formalParameter = (v \ "@formalParameter").toString
        val refLocalId = (v \ "connectionPointIn" \ "connection" \ "@refLocalId").toString
        Terminal(formalParameter, refLocalId, "", "")
      })
    }

    val funcBlocks = pou \\ "block"
    funcBlocks.map(fb => {
      val instanceName = (fb \ "@instanceName").toString
      val localId = (fb \ "@localId").toString
      val typeName = (fb \ "@typeName").toString
      val inputVars = getVars("inputVariables", fb)
      val outputVars = getVars("outputVariables", fb)
      val inoutVars = getVars("inOutVariables", fb)
      localId -> FunctionBlock(localId, instanceName, typeName, "", inputVars, outputVars, inoutVars)
    }).toMap
  }


  /**
    * POU内のFBの各端子の結線の情報を追加したFBを返す
    * @param pou Node
    * @return FBの各端子の結線の情報を追加したFBを返す
    */
  def getFunctionBlockConnection(pou: Node): Seq[FunctionBlock] = {
    val varNameMap = getVarNameMap(pou)
    val inVarMap = getInVarMap(pou)
    val outVarMap = getOutVarMap(pou)
    val functionBlocks = getFunctionBlock(pou)


    val updatedFB = functionBlocks.toList.map{case (_, fb) => {
      // 入力端子の更新
      val updatedInputVar = fb.inputVars.map(t => {
        val varName = inVarMap.getOrElse(t.refLocalId, "-")
        val varComment = varNameMap.getOrElse(varName, if(t.refLocalId != "") "定数" else if(varName == "-")"-" else "")
        t.copy(varName = varName, varComment = varComment)
      })

      // 出力端子の更新
      val updatedOutputVar = fb.outputVars.map(t => {
        val varName = outVarMap.getOrElse(ConnectionPointIn(fb.localId, t.name), "-")
        val varComment = varNameMap.getOrElse(varName, if(varName == "-")"-" else "")
        t.copy(varName = varName, varComment = varComment)
      })

      // 入出力端子の更新
      val updatedInoutVar = fb.inoutVars.map(t => {
        val varName = inVarMap.getOrElse(t.refLocalId, "-")
        val varComment = varNameMap.getOrElse(varName, if(t.refLocalId != "") "定数" else if(varName == "-")"-" else "")
        t.copy(varName = varName, varComment = varComment)
      })

      // 各端子の情報を更新したFB情報を返す
      fb.copy(comment = varNameMap.getOrElse(fb.name, "") ,inputVars = updatedInputVar, outputVars = updatedOutputVar, inoutVars = updatedInoutVar)
    }}
    updatedFB
  }


  /**
    * PO情報出力フォーマットのヘッダーを返す
    * @return
    */
  def poFormatHeader(): Seq[String] = Seq("PO/SPO", "PONO", "EPONO", "FB種別", "PO名称", "端子種別", "端子名", "変数名称", "コメント")


  /**
    * FunctionBlockを出力用のフォーマットに変換する
    * @param fb FunctionBlock
    * @return Seq[String]: 出力の表のデータ
    */
  def poFormat(fb: FunctionBlock): Seq[Seq[String]] = {
    // PO,SPOを持つFBを抽出
    if (fb.hasPO) {
      val poNo = fb.poNo
      val epoNo = fb.epoNo
      val poType = fb.poType
      val inputTerminals = fb.inputVars.filter(t => t.name != "PONO" && t.name != "EPONO" && t.name != "SPONO" && t.name != "SEPONO")
      val inputTerminalFormat = inputTerminals.map(t => Seq(poType, poNo, epoNo, fb.typeName, fb.comment, "INPUT", t.name, t.varName, t.varComment))
      val outputTerminalFormat = fb.outputVars.map(t => Seq(poType, poNo, epoNo, fb.typeName, fb.comment, "OUTPUT", t.name, t.varName, t.varComment))
      val inoutTerminalFormat = fb.inoutVars.map(t => Seq(poType, poNo, epoNo, fb.typeName, fb.comment, "INOUT", t.name, t.varName, t.varComment))
      inputTerminalFormat ++ outputTerminalFormat ++ inoutTerminalFormat
    } else {
      Seq()
    }
  }


  /**
    * PO情報出力フォーマットにしたPO情報のファイルへ書き込む内容を返す
    * @param poFormatSeq poFormatメソッドの出力のSeqをflattenしたもの
    * @return
    */
  def printPoFormat(poFormatSeq:Seq[Seq[String]]): String = {
    (poFormatHeader +: poFormatSeq.sortBy(seq => seq(1).toInt * 100 + seq(2).toInt)).filter(_.nonEmpty).map(_.mkString(",")).mkString("\n")
  }

  /**
    *
    * @param xmlFileName
    * @return ファイルに書き込めるPO情報フォーマット文字列
    */
  def apply(xmlFileName: String): String = {
    val xml = XML.loadFile(xmlFileName)
    val pous = (xml \\ "pou").filter(p => {
      val pouType = (p \ "@pouType").toString
      pouType.toString == "program"
    })
    val poformat = pous.map(pou => getFunctionBlockConnection(pou)).flatten.map(fb => poFormat(fb)).flatten.filter(_.nonEmpty)
    printPoFormat(poformat)
  }

}

