import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.geometry.Insets
import scalafx.print.PrintColor.Color
import scalafx.scene.Scene
import scalafx.scene.control.{Alert, Button}
import scalafx.scene.effect.DropShadow
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.shape.Rectangle
import scalafx.scene.text.Text
import scalafx.Includes._
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.{KeyCode, KeyEvent}

object GUIgame extends JFXApp {
  stage = new PrimaryStage {
    title = "GUI Game"
    val swidth = 600
    val sheight = 600
    scene = new Scene(swidth,sheight) {
      fill = White
      val button = new Button("Restart")
      val txt: Text = new Text{
        text = "Hello World!"
        style = "-fx-font-size: 48pt"
        fill = new LinearGradient(
          endX = 0,
          stops = Stops(PaleGreen, SeaGreen))
      }
      onKeyPressed = (ev:KeyEvent) => {
        if( ev.code == KeyCode.Left && player.getX-50 >= 0 ) {
          player.x = player.getX - 50
        }
        else if( ev.code == KeyCode.Right && player.getX+50 <= swidth-player.getWidth) {
          player.x = player.getX + 50
        }
        else if( ev.code == KeyCode.Up && player.getY-50 >= 0) {
          player.y = player.getY - 50
        }
        else if(ev.code == KeyCode.Down && player.getY+50 <= sheight-player.getHeight){
          player.y = player.getY + 50
        }
      }
      val player: Rectangle = new Rectangle{
        x = 300
        y = 300
        width = 25
        height = 25
        fill = Green
      }
      content = Seq(
        player
      )
    }
  }
}