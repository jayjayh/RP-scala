import scalafx.Includes._
import scalafx.animation.PauseTransition
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Line, Polygon}
import scalafx.util.Duration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

object GUIgame extends JFXApp {
  val cellSize = 50
  //  val t = new java.util.Timer()
  //  val task = new java.util.TimerTask {
  //    def run() = println("Beep!")
  //  }
  //  t.schedule(task, 1000L, 1000L)
  //  task.cancel()
  stage = new PrimaryStage{
    title = "GUI Game"
    height = 625
    val swidth = 550
    val sheight = 550
    var tris: Seq[Polygon] = Seq()
    val grid: Seq[Line] = createGrid(swidth,sheight)
    val player: Circle = new Circle{
      centerX= swidth/2
      centerY= sheight/2
      radius= cellSize/2
      fill = Green
    }
    val quitbutton:Button = new Button {
      text = "Quit"
      relocate(500,555)
      onAction = () => {
        stage.close()
      }
    }
    val timer = new PauseTransition(Duration(1500))
    timer.play
//    createTri(0,0,"TtB",rand.nextInt(3)+1,draw)
    def draw(n:Polygon): Unit = {
      tris = tris:+n
    }
    def move(tri:Polygon):Polygon={
      val pts = tri.getPoints
      var fwd=0
      tri.fill match{
        case Blue => fwd=2
        case Orange => fwd=1
        case Red => fwd=3
        case _ => fwd=0
      }
      var X=pts(0)/cellSize
      var Y=pts(1)/cellSize
      val dir= tri.accessibleText.toString()

      dir match{
      case "LtR" => X+=fwd
      case "RtL" => X-=fwd
      case "TtB" => Y+=fwd
      case "BtT" => Y-=fwd
      case _ => X+=fwd
      }
      triangle(X,Y,dir,fwd)
    }
    scene = new Scene(swidth,sheight) {
      fill = White
      genTri()
      onKeyPressed = (ev:KeyEvent) => {
        if( ev.code == KeyCode.Left) {
          println("left")
          println(tris)
        }
        else if( ev.code == KeyCode.Right) {
          println("right")
        }
        else if( ev.code == KeyCode.Up) {
          println("up")
        }
        else if(ev.code == KeyCode.Down){
          println("down")
        }
      }
      val restartbutton:Button = new Button {
        text = "Restart"
        relocate(10,555)
        onAction = () => {
          update()
          println("button pressed")
        }
      }

      def update():Unit ={
        tris.map(move(_))
        content = grid ++ Seq(player,restartbutton,quitbutton) ++ tris
        timer.playFromStart()
      }
      timer.onFinished = {() =>update();println("updated")}

      def genTri(): Unit ={
        val r = new Random()
        Future{
          Thread.sleep(1000)
          while(r.nextInt(6) != 0){
            Thread.sleep(1000)
          }
        }.onComplete{
          case Success(s) => {
            genRandTriangle()
            genTri()
          }
          case Failure(f) => println(f)
        }
      }
      def genRandTriangle(): Unit ={

        val rand = new Random()
        rand.nextInt(4) match{
          case 0 => createTri(0,rand.nextInt(11),"LtR",rand.nextInt(3)+1,draw); println("generated triangle")
          case 1 => createTri(rand.nextInt(11),0,"TtB",rand.nextInt(3)+1,draw); println("generated triangle")
          case 2 => createTri(10,rand.nextInt(11),"RtL",rand.nextInt(3)+1,draw); println("generated triangle")
          case 3 => createTri(rand.nextInt(11),10,"BtT",rand.nextInt(3)+1,draw); println("generated triangle")
          case _ => println("Error")
        }
      }
      content = grid ++ Seq(player,restartbutton,quitbutton) ++ tris

    }
  }

  def createTri(x:Double,y:Double,dir:String,step:Int,d:Polygon => Unit): Unit = {
    val tri= Future{
      triangle(x,y,dir,step)
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
  def triangle(x:Double, y:Double,dir:String,steps:Int): Polygon = {
    val triangle = new Polygon()
    triangle.accessibleText=dir
    dir match {
      case "LtR" => triangle.getPoints.setAll(0+(x*cellSize), 0+(y*cellSize), 0+(x*cellSize), cellSize+(y*cellSize), cellSize+(x*cellSize), (cellSize/2)+(y*cellSize))
      case "RtL" => triangle.getPoints.setAll(cellSize+(x*cellSize), 0+(y*cellSize), cellSize+(x*cellSize), cellSize+(y*cellSize), 0+(x*cellSize), (cellSize/2)+(y*cellSize))
      case "TtB" => triangle.getPoints.setAll(0+(x*cellSize), 0+(y*cellSize), (cellSize/2)+(x*cellSize), cellSize+(y*cellSize), cellSize+(x*cellSize), 0+(y*cellSize))
      case "BtT" => triangle.getPoints.setAll(0+(x*cellSize), cellSize+(y*cellSize), (cellSize/2)+(x*cellSize), 0+(y*cellSize), cellSize+(x*cellSize), cellSize+(y*cellSize))
      case _ => triangle.getPoints.setAll(0+(x*cellSize), 0+(y*cellSize), 0+(x*cellSize), cellSize+(y*cellSize), cellSize+(x*cellSize), (cellSize/2)+(y*cellSize))
    }
    steps match{
      case 1=>triangle.fill= Orange
      case 2=>triangle.fill= Blue
      case 3=>triangle.fill= Red
      case _=>triangle.fill= Orange
    }
    triangle
  }
}