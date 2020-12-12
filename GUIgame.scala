import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.paint.Color._
import scalafx.scene.paint.{LinearGradient, Stops}
import scalafx.scene.shape.{Circle, Line, Polygon}
import scalafx.scene.text.Text

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object GUIgame extends JFXApp {
  val cellSize = 50
//  val t = new java.util.Timer()
//  val task = new java.util.TimerTask {
//    def run() = println("Beep!")
//  }
//  t.schedule(task, 1000L, 1000L)
//  task.cancel()

  stage = new PrimaryStage {
    title = "GUI Game"
    val swidth = 550
    val sheight = 550

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

//      onKeyPressed = (ev:KeyEvent) => {
//        if( ev.code == KeyCode.Left && player.getX-50 >= 0 ) {
//          player.x = player.getX - 50
//        }
//        else if( ev.code == KeyCode.Right && player.getX+50 <= swidth-player.getWidth) {
//          player.x = player.getX + 50
//        }
//        else if( ev.code == KeyCode.Up && player.getY-50 >= 0) {
//          player.y = player.getY - 50
//        }
//        else if(ev.code == KeyCode.Down && player.getY+50 <= sheight-player.getHeight){
//          player.y = player.getY + 50
//        }
//      }

      val grid: Seq[Line] = createGrid(swidth,sheight)
      val player: Circle = new Circle{
        centerX= swidth/2
        centerY= sheight/2
        radius= cellSize/2
        fill = Green
      }
      val tris: Seq[Polygon]=Seq(triangle(0,1,"LtR"))
      createTri(0,0,"TtB",draw _)
      content = grid ++ Seq(
        player) ++ tris

      def draw(n:Polygon)={
        tris:+n
        tris
      }
    }
  }

  def createTri(x:Double,y:Double,dir:String,d:Polygon => Unit) = {
    val tri= Future{
      triangle(x,y,"LtR")
    }
      tri.onComplete {
        case Failure(ex) => println(ex.getMessage)
        case Success(v) => d(v)
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