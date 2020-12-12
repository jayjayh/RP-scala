import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.shape.{Line, Polygon, Rectangle}
import scalafx.scene.text.Text

object GUIgame extends JFXApp {
  val cellSize = 50
  stage = new PrimaryStage {
    title = "GUI Game"
    val swidth = 500
    val sheight = 500

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

      val grid: Seq[Line] = createGrid(swidth,sheight)

      val player: Rectangle = new Rectangle{
        x = 300
        y = 300
        width = cellSize
        height = cellSize
        fill = Green
      }

      content = grid ++ Seq(
        player,triangle(4,4,"RtL")
      )
    }
  }
  def createGrid(height:Int,width:Int):Seq[Line] = {
    var xgrid: Seq[Line] = Seq()
    var ygrid: Seq[Line] = Seq()
    xgrid = for (n <- (0 to height).by(cellSize)) yield new Line {startX = 0; startY = n; endX = width; endY = n}
    ygrid = for (n <- (0 to width).by(cellSize)) yield new Line {startX = n; startY = 0; endX = n; endY = height}
    xgrid++ygrid
  }
  def triangle(x:Double, y:Double,dir:String) = {
    val triangle = new Polygon()
    dir match {
      case "LtR" => triangle.getPoints.setAll(0+(x*cellSize), 0+(y*cellSize), 0+(x*cellSize), cellSize+(y*cellSize), cellSize+(x*cellSize), (cellSize/2)+(y*cellSize))
      case "RtL" => triangle.getPoints.setAll(cellSize+(x*cellSize), 0+(y*cellSize), cellSize+(x*cellSize), cellSize+(y*cellSize), 0+(x*cellSize), (cellSize/2)+(y*cellSize))
      case "TtB" => triangle.getPoints.setAll(0+(x*cellSize), 0+(y*cellSize), (cellSize/2)+(x*cellSize), cellSize+(y*cellSize), cellSize+(x*cellSize), 0+(y*cellSize))
      case "BtT" => triangle.getPoints.setAll(0+(x*cellSize), cellSize+(y*cellSize), (cellSize/2)+(x*cellSize), 0+(y*cellSize), cellSize+(x*cellSize), cellSize+(y*cellSize))
      case _ => triangle.getPoints.setAll(0+(x*cellSize), 0+(y*cellSize), 0+(x*cellSize), cellSize+(y*cellSize), cellSize+(x*cellSize), (cellSize/2)+(y*cellSize))
    }
    triangle.fill= Orange
    triangle
  }
}