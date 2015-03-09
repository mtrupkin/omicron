package me.mtrupkin.controller

import javafx.collections.FXCollections._
import javafx.fxml.FXML
import javafx.scene.control.{Label, TableColumn, TableView}
import javafx.scene.layout.Pane
import me.mtrupkin.control.ConsoleFx
import me.mtrupkin.console._
import me.mtrupkin.core.{Point, Points}
import me.mtrupkin.game.model.{CombatTracker, Agent, World}

import scalafx.beans.property.StringProperty
import scalafx.scene.{control => sfxc}
import scalafx.scene.{layout => sfxl}
import scalafx.scene.{input => sfxi}

import scala.collection.JavaConversions._
import scalafx.Includes._

/**
 * Created by mtrupkin on 12/15/2014.
 */
trait Game { self: Controller =>
  class GameController(val tracker: CombatTracker) extends ControllerState {
    implicit def itos(int: Int): String = int.toString
    val name = "Game"

    @FXML var trackerTbl: TableView[AgentBean] = _
    @FXML var nameCol:  TableColumn[AgentBean, String] = _
    @FXML var hpCol:  TableColumn[AgentBean, String] = _
    @FXML var strText: Label = _
    @FXML var dexText: Label = _
    @FXML var intText: Label = _
    @FXML var actionsText: Label = _
    @FXML var hpText: Label = _
    @FXML var infoText: Label = _
    @FXML var infoDescText: Label = _
    @FXML var infoPosText: Label = _
    @FXML var consolePane: Pane = _
    @FXML var rootPane: Pane = _

    var console: ConsoleFx = _
    var screen: Screen = _

    def initialize(): Unit = {
      val consoleSize = tracker.world.tileMap.size
      console = new ConsoleFx(consoleSize)
      console.setStyle("-fx-border-color: white")
      new sfxl.Pane(console) {
        onMouseClicked = (e: sfxi.MouseEvent) => handleMouseClicked(e)
        onMouseMoved = (e: sfxi.MouseEvent) => handleMouseMove(e)
        onMouseExited = (e: sfxi.MouseEvent) => handleMouseExit(e)
      }

      screen = Screen(consoleSize)
      consolePane.getChildren.clear()
      consolePane.getChildren.add(console)
      consolePane.setFocusTraversable(true)
      new sfxl.Pane(rootPane) {
        filterEvent(sfxi.KeyEvent.Any) {
          (event: sfxi.KeyEvent) => handleKeyPressed(event)
        }
      }
      

      trackerTbl.setPlaceholder(new Label)
      trackerTbl.setMouseTransparent(true)

      nameCol.prefWidthProperty().bind(trackerTbl.widthProperty().multiply(0.75))
      hpCol.prefWidthProperty().bind(trackerTbl.widthProperty().multiply(0.20))

      new sfxc.TableColumn(nameCol).cellValueFactory = { _.value.name }
      new sfxc.TableColumn(hpCol).cellValueFactory = { _.value.hp }

      timer.start()
    }

    override def update(elapsed: Int): Unit = {
      import tracker.world.player._

      val agentModel = observableArrayList[AgentBean](tracker.agents.map(a => new AgentBean(a)))
      trackerTbl.setItems(agentModel)

      strText.setText(stats.str)
      dexText.setText(stats.dex)
      intText.setText(stats.int)
      actionsText.setText(tracker.player.ap)
      hpText.setText(hp)

      // TODO: update and render at different rates
      tracker.update(elapsed)
      tracker.render(screen)

      console.draw(screen)
      if (tracker.end) {

        changeState(new OutroController)
      }
    }

    implicit def pointToString(p: Point): String = {
      s"[${p.x}, ${p.y}]"
    }

    def handleMouseClicked(mouseEvent: sfxi.MouseEvent): Unit = {
      for( p <- mouseToPoint(mouseEvent)) {
        tracker.target(p)
      }
    }

    def handleMouseMove(mouseEvent: sfxi.MouseEvent): Unit = {
      for( p <- mouseToPoint(mouseEvent)) {
        infoPosText.setText(p)
        tracker.mouse = Some(p)

        val actionOpt = tracker.getAction(p)
        actionOpt match {
          case Some(action) => infoDescText.setText(action.name)
          case None => infoDescText.setText("")
        }

        val target = tracker.agents.find(a => a.position == p)
        target match {
          case Some(t) => infoText.setText(t.name)
          case None => infoText.setText(tracker.world.tileMap(p).name)
        }
      }
    }

    def handleMouseExit(mouseEvent: sfxi.MouseEvent): Unit = {
      tracker.mouse = None
      infoText.setText("")
      infoDescText.setText("")
      infoPosText.setText("")
    }

    def mouseToPoint(mouseEvent: sfxi.MouseEvent): Option[Point] = console.toScreen(mouseEvent.x, mouseEvent.y)

    def handleKeyPressed(event: sfxi.KeyEvent): Unit = {
      import me.mtrupkin.console.Key._
      val key = keyCodeToConsoleKey(event)
      key match {
        case ConsoleKey(Q, Modifiers.Control) => {
          World.write(tracker.world)
          changeState(new IntroController)
        }
        case ConsoleKey(X, Modifiers.Control) => exit()
        case ConsoleKey(k, _) => k match {
          case Space => tracker.passTurn()
          case Esc => flipState(new HelpController)
          case _ =>
        }
        case _ =>
      }
    }
  }

  def keyCodeToConsoleKey(event: sfxi.KeyEvent): ConsoleKey = {
    val modifier = Modifier(event.shiftDown, event.controlDown, event.altDown)
    val jfxName = event.code.name
    val key = Key.withName(jfxName)
    ConsoleKey(key, modifier)
  }

  class AgentBean(agent: Agent) {
    val name = new StringProperty(this, "name", agent.name)
    val hp = new StringProperty(this, "hp", agent.hp.toString)
  }
}

