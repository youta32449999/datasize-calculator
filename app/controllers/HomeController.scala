package controllers

import javax.inject.Inject
import javax.inject.Singleton

import play.api.mvc.MessagesAbstractController
import play.api.mvc.MessagesControllerComponents


@Singleton
class HomeController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  def index() = Action {implicit request =>
    Ok(views.html.index())
  }
}
