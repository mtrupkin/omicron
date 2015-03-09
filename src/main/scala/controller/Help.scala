package me.mtrupkin.controller

import java.nio.file.{Paths, Files}
import javafx.fxml.FXML
import javafx.scene.control.TabPane
import javafx.scene.layout.Pane
import javafx.scene.web.WebView


import me.mtrupkin.console.{Modifiers, ConsoleKey}
import me.mtrupkin.game.model.World
import org.pegdown.PegDownProcessor

import scalafx.scene.{control => sfxc, layout => sfxl, input => sfxi, web => sfxw, Parent}
import scalafx.Includes._


/**
 * Created by mtrupkin on 1/23/2015.
 */
trait Help {
  self: Controller =>

  class HelpController extends ControllerState {
    val name = "Help"

    @FXML var tabPane: TabPane = _
    @FXML var intro: WebView = _
    @FXML var combat: WebView = _
    @FXML var controls: WebView = _
    var sfxTabPane: sfxc.TabPane = _

    def initialize(): Unit = {
      initWebView(intro, "intro")
      initWebView(combat, "combat")
      initWebView(controls, "controls")

      sfxTabPane = new sfxc.TabPane(tabPane) {
        onKeyPressed = (e: sfxi.KeyEvent) => handleKeyPressed(e)
        filterEvent(sfxi.MouseEvent.Any) {
          (me: sfxi.MouseEvent) => filter(me)
        }
      }
    }

    var closing = false
    def filter(me: sfxi.MouseEvent): Unit = {
      if (closing) me.consume
    }

    def toHtml(htmlBody: String): String = {
      val url = getClass.getResource(s"/help/dark.css")
      val path = Paths.get(url.toURI)
      val cssContent = new String(Files.readAllBytes(path), "UTF8")
      s"""<html>
          <head>
          <style>
            $cssContent
          </style>
          </head>
          <body>
            $htmlBody
          </body>
        </html>"""
    }

    def initWebView(webView: WebView, markdown: String): Unit = {
      val processor = new PegDownProcessor
      val url = getClass.getResource(s"/help/$markdown.markdown")
      val path = Paths.get(url.toURI)
      val markdownContent = new String(Files.readAllBytes(path), "UTF8")
      val markdownHtmlBody = processor.markdownToHtml(markdownContent)
      val html = toHtml(markdownHtmlBody)

      webView.getEngine.loadContent(html)
    }

    def cleanUp(webView: WebView): Unit = {
      webView.setDisable(true)
    }

    def quit(): Unit = {
      closing = true

      cleanUp(intro)
      cleanUp(combat)
      cleanUp(controls)

      revertState()
    }

    def handleKeyPressed(event: sfxi.KeyEvent): Unit = {
      import me.mtrupkin.console.Key._
      val key = keyCodeToConsoleKey(event)
      key match {
        case ConsoleKey(Q, Modifiers.Control) => quit()
        case ConsoleKey(X, Modifiers.Control) => {
          //closed = true
        }
        case ConsoleKey(k, _) => k match {
          case Enter | Esc | Space => quit()
          case _ =>
        }
        case _ =>
      }
    }
  }

}
