/**
 * Copyright (C) 2016 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package workshop4.assignments

import javafx.application.Application
import javafx.event.ActionEvent
import javafx.geometry.Insets
import javafx.scene.Scene
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.layout.VBox
import javafx.stage.Stage

import rx.observables.JavaFxObservable
import rx.lang.scala.JavaConverters._
import rx.lang.scala.Observable

case class Credentials(username: String, password: String)

class Login extends Application {

  def start(stage: Stage) = {
    val usernameTF = new TextField {
      setPromptText("username")
      setMinHeight(30)
    }
    val passwordTF = new TextField {
      setPromptText("password")
      setMinHeight(30)
    }
    val button = new Button("Login")
    val label = new Label("<not logged in yet>")

    val root = new VBox(8, usernameTF, passwordTF, button, label) {
      setPadding(new Insets(10))
    }

    val usernameObs: Observable[String] = JavaFxObservable.fromObservableValue(usernameTF.textProperty()).asScala
    val passwordObs: Observable[String] = JavaFxObservable.fromObservableValue(passwordTF.textProperty()).asScala
    val buttonObs: Observable[ActionEvent] = JavaFxObservable.fromNodeEvents(button, ActionEvent.ACTION).asScala

    val credentialsObs = usernameObs.combineLatestWith(passwordObs)((username, password) => Credentials(username, password))
    val logins = buttonObs.withLatestFrom(credentialsObs)((_, creds) => creds)

    logins.subscribe(creds => label.setText(s"latest login: $creds"))

    stage.setScene(new Scene(root))
    stage.setTitle("Login")
    stage.show()
  }
}

object Login {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Login])
  }
}
