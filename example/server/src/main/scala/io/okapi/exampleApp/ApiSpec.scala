package io.okapi.exampleApp

import io.okapi.core.Okapi

/** The OpenAPI document of the example's REST controllers. */
object ApiSpec {
  type RestControllers = (BookController, UserController, ExploreController, CoverController, AdminController)

  def yaml: String = Okapi.openApiYaml[RestControllers]("Okapi Example API", "1.0.0")
}
