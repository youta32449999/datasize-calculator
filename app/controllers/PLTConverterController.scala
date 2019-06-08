package controllers

import java.io.PrintWriter
import java.nio.file.{Files, Paths}

import javax.inject.{Inject, Singleton}
import play.api.mvc.{MessagesAbstractController, MessagesControllerComponents}

import scala.concurrent.ExecutionContext

@Singleton
class PLTConverterController @Inject()(cc: MessagesControllerComponents)(implicit ec: ExecutionContext) extends MessagesAbstractController(cc) {
  import services._

  def index() = Action {implicit request =>
    Ok(views.html.plt_convert.home())
  }

  def upload() = Action(parse.multipartFormData) { request =>

    request.body.file("plt_file") match {
      case Some(uploadFile) => {

        // uploadされたfileの名前を取得
        val fileName = uploadFile.filename

        // uploadされたfileを一時的に保存
        uploadFile.ref.copyTo(Paths.get(s"./tmp/cache/$fileName"), replace = true)

        // pltをコンバート
        val result = PLTConverter(s"./tmp/cache/$fileName")

        // 新規ファイル作成
        val file = Paths.get(s"./tmp/converted/$fileName")
        if (Files.notExists(file)) Files.createFile(file)

        // 書き込み
        val pw = new PrintWriter(s"./tmp/converted/$fileName")
        pw.write(result)
        pw.close

        // 結果をダウンロード
        Redirect(routes.PLTConverterController.download(s"./tmp/converted/${fileName}"))
      }
      case None => Redirect(routes.PLTConverterController.index)
    }
  }

  def download(fileName: String) = Action {implicit request =>
    Ok.sendFile(new java.io.File(fileName))
  }
}
