package controllers

import javax.inject.Inject
import javax.inject.Singleton

import play.api.mvc.MessagesAbstractController
import play.api.mvc.MessagesControllerComponents


@Singleton
class DataSizeCalculatorController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {
  import services.DataTypeCalculator
  import services.DataTypeParser
  import services.DataTypeParser._

  def index() = Action {implicit request =>
    Ok(views.html.datasize_calculate.home())
  }

  def calculateResult() = Action {implicit request =>
    val form: Option[Map[String, Seq[String]]] = request.body.asFormUrlEncoded
    val formData: Map[String, Seq[String]] = form.getOrElse(Map())
    val userDataType: String = formData.getOrElse("userdatatype", List("")).head
    val parseResult: Map[String, Int] = DataTypeParser(userDataType) match {
      case Success(result, _) => DataTypeCalculator(result)
      case _ => Map()
    }

    Ok(views.html.datasize_calculate.result(parseResult.toList))
  }
}
