import scalafx.Includes._
import scalafx.animation.PauseTransition
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.text.Text
//import scalafx.event.ActionEvent.Any
import scalafx.event.{ActionEvent, Event}
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Line, Polygon}
//import scalafx.scene.text.Text
import scalafx.util.Duration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

object GUIgame extends JFXApp {
  val cellSize:Double = 50
  stage = new PrimaryStage{
    title = "GUI Game"
    height = 625
    var triggered = false
    val swidth = 550
    val sheight = 550
    var gameover:Boolean = false
    var tris: Seq[Polygon] = Seq()
    val grid: Seq[Line] = createGrid(swidth,sheight)
//    val player: Circle = new Circle{
//      centerX= swidth/2
//      centerY= sheight/2
//      radius= cellSize/2
//      fill = Green
//    }
    val quitbutton:Button = new Button {
      text = "Quit"
      onAction = () => {
        stage.close()
      }
    }
    val timer = new PauseTransition(Duration(200))
    timer.play
    //    createTri(0,0,"TtB",rand.nextInt(3)+1,draw)
    val draw= (n:Polygon)=> {
      tris = tris:+n
      n.fireEvent(new ActionEvent())
    }
    scene = new Scene(swidth,sheight) {
      fill = White
      //genTri()
      val startButton:Button = new Button {
        text = "Start Game"
        onAction = (ev: Event) => {
          println(ev)
          if (!triggered) {
            genTri()
            triggered = true
            gameover = false
//            println("Mouse enter, genTri()")
          }
        }
      }
      val restartbutton:Button = new Button {
        text = "Restart"
        translateX=5
        onAction = () => {
          points=0
          gameover = true
          triggered = false
          println("Restarted")
        }
      }
      var points=0;
      val addPoint= (tb:Text,p:Int)=>{
        if(!gameover) {
          points+=p
          tb.text="Score: " +points.toString
        }

      }
      val score: Text = new Text{
        //translateX = 50
        text = "Score: " + points
      }
      val hbox:HBox = new HBox{

        layoutY = 555

//        val time: Text = new Text{
//                  translateX = 200
//                  text = "Time: " + "temp time"
//                }
        quitbutton.translateX = 330
        startButton.translateX = 175
        children = Seq(restartbutton,startButton,score/*,time*/,quitbutton)
      }
//      onKeyPressed = (ev:KeyEvent) => {
//        ev.code match{
//          case KeyCode.Left =>  println(tris)
//          case KeyCode.Right => println(gameover)
//          case KeyCode.Up => player.layoutY = player.getLayoutY - 50
//          case KeyCode.Down=> player.layoutY = player.getLayoutY + 50
//        }
//      }

      val update=()=>{
        if(!gameover){
          content = grid ++ Seq(hbox) ++ tris
          timer.playFromStart()
        }
      }
      timer.onFinished = {() =>update()}
      //      onMouseClicked = {
      //        () => {
      //          println("clicked")
      //          points = points + 1
      //          println(points)
      //        }
      //      }
      def genTri(): Unit ={
        val r = new Random()
        Future{
          //          Thread.sleep(1000)
          while(r.nextInt(4) != 0){
            Thread.sleep(1000)
          }
        }.onComplete{
          case Success(s) => {
            if(!gameover) {
              genTri()
              genRandTriangle(addPoint(score,_))
            }
          }
          case Failure(f) => println(f)
        }
      }
      def delete(d:Polygon):Unit ={
        tris = tris.filterNot(_.eq(d))
      }
      def value(d:Polygon):List[Double] ={
        val dir = d.userData
        val col = d.getFill
        var xy:List[Double]= List()

        dir match {
          case "LtR" => xy = xy :+ cellSize; xy = xy :+ 0.0
          case "RtL" => xy = xy :+ -cellSize; xy = xy :+ 0.0
          case "TtB" => xy = xy :+ 0.0; xy = xy :+ cellSize
          case "BtT" => xy = xy :+ 0.0; xy = xy :+ -cellSize
          case _ => xy = xy :+ 0.0; xy = xy :+ 0.0
        }
        col match{
          case Orange => xy = xy :+ 1000.0
          case Blue => xy = xy :+ 500.0
          case Red => xy = xy :+ 300.0
          case _ => xy = xy :+ 3000.0
        }
        xy
      }
      def genRandTriangle(cb:(Int)=>Unit): Unit ={
        Future {
          var tri: Polygon = null
          val rand = new Random()
          rand.nextInt(4) match {
            case 0 => tri = createTri(0, rand.nextInt(11), "LtR", rand.nextInt(3) + 1,draw)//; println("generated triangle")
            case 1 => tri = createTri(rand.nextInt(11), 0, "TtB", rand.nextInt(3) + 1,draw)//; println("generated triangle")
            case 2 => tri = createTri(10, rand.nextInt(11), "RtL", rand.nextInt(3) + 1,draw)//; println("generated triangle")
            case 3 => tri = createTri(rand.nextInt(11), 10, "BtT", rand.nextInt(3) + 1,draw)//; println("generated triangle")
            case _ => println("Error")
          }
          var xxx = tri.getPoints
          val v = value(tri)
          val movex:Double = v.head
          val movey = v(1)
          val speed = v(2)
          while(!gameover && (0) <= swidth+(cellSize) && xxx(0) >= 0 && xxx(1) <= sheight+(cellSize) && xxx(1) >= 0) {
            tri.getPoints.setAll(xxx(0) + movex,xxx(1) + movey,xxx.get(2)+ movex,xxx(3) + movey,xxx(4)+movex,xxx(5) + movey)
            xxx = tri.getPoints
            Thread.sleep(speed.toInt)
            //println(x + " " + y + " bounds: x<=" + (swidth+(cellSize)) + " y<=" + (sheight+(cellSize)))
          }
          tri
        }.onComplete{
          case Success(v) => /*println("triangle done")*/cb(1);delete(v);
          case Failure(ex) => println(ex.getMessage + "failed")
        }
      }
      content = grid ++ Seq(hbox) ++ tris

    }
    def createTri(x:Double,y:Double,dir:String,step:Int,d:Polygon => Unit): Polygon = {
      triangle(x,y,dir,step,d)
    }
    def createGrid(height:Int,width:Int):Seq[Line] = {
      var xgrid: Seq[Line] = Seq()
      var ygrid: Seq[Line] = Seq()
      xgrid = for (n <- (0 to height).by(cellSize.toInt)) yield new Line {startX = 0; startY = n; endX = width; endY = n}
      ygrid = for (n <- (0 to width).by(cellSize.toInt)) yield new Line {startX = n; startY = 0; endX = n; endY = height}
      xgrid++ygrid
    }
    def triangle(x:Double, y:Double,dir:String,steps:Int,d:Polygon => Unit): Polygon = {
      val triangle = new Polygon()
      triangle.userData = dir
      triangle.onMouseEntered() = {() => println("Triangle touched");triggered = false;gameover = true}
      dir match {
        case "LtR" => triangle.getPoints.setAll(1+(x*cellSize), 1+(y*cellSize), 1+(x*cellSize), cellSize-1+(y*cellSize), cellSize-1+(x*cellSize), (cellSize/2)-.5+(y*cellSize))
        case "RtL" => triangle.getPoints.setAll(cellSize-1+(x*cellSize), 1+(y*cellSize), cellSize-1+(x*cellSize), cellSize-1+(y*cellSize), 1+(x*cellSize), (cellSize/2)-.5+(y*cellSize))
        case "TtB" => triangle.getPoints.setAll(1+(x*cellSize), 1+(y*cellSize), (cellSize/2)-.5+(x*cellSize), cellSize-1+(y*cellSize), cellSize-1+(x*cellSize), 1+(y*cellSize))
        case "BtT" => triangle.getPoints.setAll(1+(x*cellSize), cellSize-1+(y*cellSize), (cellSize/2)-.5+(x*cellSize), 1+(y*cellSize), cellSize-1+(x*cellSize), cellSize-1+(y*cellSize))
        case _ => triangle.getPoints.setAll(1+(x*cellSize), 1+(y*cellSize), 1+(x*cellSize), cellSize-1+(y*cellSize), cellSize-1+(x*cellSize), (cellSize/2)-.5+(y*cellSize))
      }
      steps match{
        case 1=>triangle.fill= Orange
        case 2=>triangle.fill= Blue
        case 3=>triangle.fill= Red
        case _=>triangle.fill= Orange
      }
      d(triangle)
      triangle
    }
  }
}