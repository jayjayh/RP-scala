//import javafx.collections.ObservableList
//import javafx.scene.Node
import scalafx.Includes._
import scalafx.animation.PauseTransition
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
//import scalafx.event.ActionEvent.Any
import scalafx.event.{ActionEvent, Event}
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import scalafx.scene.layout.HBox
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Line, Polygon}
//import scalafx.scene.text.Text
import scalafx.util.Duration

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

object GUIgame extends JFXApp {
  val cellSize = 50
  stage = new PrimaryStage{
    title = "GUI Game"
    height = 625
    var triggered = false
    val swidth = 550
    val sheight = 550
    var points = 0
    var gameover:Boolean = false
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
      onAction = () => {
        stage.close()
      }
    }
    val timer = new PauseTransition(Duration(200))
    timer.play
    //    createTri(0,0,"TtB",rand.nextInt(3)+1,draw)
    def draw(n:Polygon): Unit = {
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
            println("Mouse enter, genTri()")
          }
        }
      }
      val restartbutton:Button = new Button {
        text = "Restart"
        onAction = () => {
          gameover = true
          triggered = false
          println("Restarted")
        }
      }
      val hbox:HBox = new HBox{

        layoutY = 555
//        val score: Text = new Text{
//          //translateX = 50
//          text = "Score: " + points
//        }
//        val time: Text = new Text{
//          translateX = 200
//          text = "Time: " + "temp time"
//        }
        quitbutton.translateX = 350
        startButton.translateX = 175
        children = Seq(restartbutton,startButton/*,score,time*/,quitbutton)
      }
      onKeyPressed = (ev:KeyEvent) => {
        ev.code match{
          case KeyCode.Left =>  println(tris)
          case KeyCode.Right => println(gameover)
          case KeyCode.Up => player.layoutY = player.getLayoutY - 50
          case KeyCode.Down=> player.layoutY = player.getLayoutY + 50
        }
      }

      def update():Unit ={

        content = grid ++ Seq(hbox) ++ tris
        timer.playFromStart()
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
            Thread.sleep(1500)
          }
        }.onComplete{
          case Success(s) => {
            if(!gameover) {
              genTri()
              genRandTriangle()
            }
          }
          case Failure(f) => println(f)
        }
      }
      def delete(d:Polygon):Unit ={
        tris = tris.filterNot(_.eq(d))
      }
      def value(d:Polygon):List[Int] ={
        val dir = d.userData
        val col = d.getFill
        var xy:List[Int]= List()

        dir match {
          case "LtR" => xy = xy :+ cellSize; xy = xy :+ 0
          case "RtL" => xy = xy :+ -cellSize; xy = xy :+ 0
          case "TtB" => xy = xy :+ 0; xy = xy :+ cellSize
          case "BtT" => xy = xy :+ 0; xy = xy :+ -cellSize
          case _ => xy = xy :+ 0; xy = xy :+ 0
        }
        col match{
          case Orange => xy = xy :+ 1000
          case Blue => xy = xy :+ 500
          case Red => xy = xy :+ 300
          case _ => xy = xy :+ 3000
        }
        xy
      }
      def genRandTriangle(): Unit ={
        Future {
          var tri: Polygon = null
          val rand = new Random()
          rand.nextInt(4) match {
            case 0 => tri = createTri(0, rand.nextInt(11), "LtR", rand.nextInt(3) + 1,draw); println("generated triangle")
            case 1 => tri = createTri(rand.nextInt(11), 0, "TtB", rand.nextInt(3) + 1,draw); println("generated triangle")
            case 2 => tri = createTri(10, rand.nextInt(11), "RtL", rand.nextInt(3) + 1,draw); println("generated triangle")
            case 3 => tri = createTri(rand.nextInt(11), 10, "BtT", rand.nextInt(3) + 1,draw); println("generated triangle")
            case _ => println("Error")
          }
          var xxx = tri.getPoints
          val v = value(tri)
          val movex:Int = v.head
          val movey = v(1)
          val speed = v(2)
          while(xxx(0) <= swidth+(cellSize) && xxx(0) >= 0 && xxx(1) <= sheight+(cellSize) && xxx(1) >= 0) {
            tri.getPoints.setAll(xxx(0) + movex,xxx(1) + movey,xxx.get(2)+ movex,xxx(3) + movey,xxx(4)+movex,xxx(5) + movey)
            xxx = tri.getPoints
            Thread.sleep(speed)
            //println(x + " " + y + " bounds: x<=" + (swidth+(cellSize)) + " y<=" + (sheight+(cellSize)))
          }
          tri
        }.onComplete{
          case Success(v) => println("triangle done");delete(v);
          case Failure(ex) => println(ex.getMessage + "failed")
        }
      }
      content = grid ++ Seq(hbox) ++ tris

    }
    def createTri(x:Double,y:Double,dir:String,step:Int,d:Polygon => Unit): Polygon = {
      triangle(x,y,dir,step,draw)
    }
    def createGrid(height:Int,width:Int):Seq[Line] = {
      var xgrid: Seq[Line] = Seq()
      var ygrid: Seq[Line] = Seq()
      xgrid = for (n <- (0 to height).by(cellSize)) yield new Line {startX = 0; startY = n; endX = width; endY = n}
      ygrid = for (n <- (0 to width).by(cellSize)) yield new Line {startX = n; startY = 0; endX = n; endY = height}
      xgrid++ygrid
    }
    def triangle(x:Double, y:Double,dir:String,steps:Int,d:Polygon => Unit): Polygon = {
      val triangle = new Polygon()
      triangle.userData = dir
      triangle.onMouseEntered() = {() => println("Triangle touched");triggered = false;gameover = true}
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
      draw(triangle)
      triangle
    }
  }
}

/*

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
    val timer = new PauseTransition(Duration(1500))
    timer.play
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
    createTri(0,0,"TtB",draw)
    def draw(n:Polygon): Unit = {
      tris = tris:+n
    }
    def printXY(tri:Polygon)={
      println(tri.getPoints)
    }
    scene = new Scene(swidth,sheight) {
      fill = White
      genTri()
//      onKeyPressed = (ev:KeyEvent) => {
//        if( ev.code == KeyCode.Left) {
//          println("left")
//          println(tris)
//        }
//        else if( ev.code == KeyCode.Right) {
//          println("right")
//        }
//        else if( ev.code == KeyCode.Up) {
//          println("up")
//        }
//        else if(ev.code == KeyCode.Down){
//          println("down")
//        }
//      }
      val restartbutton:Button = new Button {
        text = "Restart"
        relocate(10,555)
        onAction = () => {
          println("button pressed")
          tris.head.layoutX = tris.head.getLayoutX + 50
        }
      }
      def update():Unit ={
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
          case 0 => createTri(0,rand.nextInt(11),"LtR",draw); println("generated triangle")
          case 1 => createTri(rand.nextInt(11),0,"TtB",draw); println("generated triangle")
          case 2 => createTri(10,rand.nextInt(11),"RtL",draw); println("generated triangle")
          case 3 => createTri(rand.nextInt(11),10,"BtT",draw); println("generated triangle")
          case _ => println("Error")
        }
      }
      content = grid ++ Seq(player,restartbutton,quitbutton) ++ tris

    }
  }

  def createTri(x:Double,y:Double,dir:String,d:Polygon => Unit): Unit = {
    val tri= Future{
      triangle(x,y,dir)
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
  def triangle(x:Double, y:Double,dir:String): Polygon = {
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

*/