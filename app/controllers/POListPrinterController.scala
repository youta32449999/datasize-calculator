package controllers

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import javax.inject.{Inject, Singleton}
import play.api.mvc.{MessagesAbstractController, MessagesControllerComponents}

import scala.concurrent.ExecutionContext

@Singleton
class POListPrinterController @Inject()(cc: MessagesControllerComponents)(implicit ec: ExecutionContext) extends MessagesAbstractController(cc) {

  import services._

  def index() = Action {implicit request =>
    Ok(views.html.polist_printer.home())
  }

  def upload() = Action(parse.multipartFormData) { request =>

    request.body.file("plc_file") match {
      case Some(uploadFile) => {

        // "./tmp/plc/"を作成
        val dir = Paths.get("./tmp/plc")
        if(Files.notExists(dir)) Files.createDirectories(dir)

        // uploadされたfileの名前を取得
        val fileName = uploadFile.filename

        // uploadされたfileを一時的に保存
        uploadFile.ref.copyTo(Paths.get(s"./tmp/plc/$fileName"), replace = true)

        // xmlからPO情報を取り出す
        val result = POListPrinter(s"./tmp/plc/$fileName")

        // 新規ファイル作成
        val resultFileName = fileName.replaceAll(".xml", ".csv")
        val file = Paths.get(s"./tmp/plc/${resultFileName}")
        if (Files.notExists(file)) Files.createFile(file)

        // 書き込み
        val pw = new PrintWriter(s"./tmp/plc/$resultFileName", "Shift-JIS")
        pw.write(result)
        pw.close

        // 結果をダウンロード
        Redirect(routes.POListPrinterController.download(s"./tmp/plc/${resultFileName}"))
      }
      case None => Redirect(routes.POListPrinterController.index)
    }
  }

  def download(fileName: String) = Action {implicit request =>
    Ok.sendFile(new java.io.File(fileName))
  }
}
