package com.harvey.ui

import com.harvey.domain.Facade
import com.harvey.domain.util.Logger
import scala.swing.{Frame,MainFrame}
import scala.swing.{BoxPanel,Button,Dialog,Orientation}
import scala.swing.ScrollPane
import scala.swing.TextArea
import scala.swing.event.ButtonClicked
import javax.swing.SwingUtilities
import javax.swing.text.DefaultCaret

class GuiFacade {
  var platform: Facade = _
  val rows = 20
  val columns = 50
  val area = new TextArea(rows, columns)
  area.peer.getCaret.asInstanceOf[DefaultCaret].setUpdatePolicy(DefaultCaret.ALWAYS_UPDATE)
  Logger.attach((message: String) => {
    area.append(message)
  })
  
  def top(): Frame = new MainFrame {
    val startButton = new Button {
      text = "start"
    }
    val stopButton = new Button {
      text = "stop"
    }
    
    // set up the content
    title = "harvey"
    val panel = new BoxPanel(Orientation.Vertical) {
      contents += new BoxPanel(Orientation.Horizontal) {
        contents += startButton
        contents += stopButton
      }
      contents += new ScrollPane(area)
    }
    contents = panel
    
    // set up the event handlers
    listenTo(startButton, stopButton)
    reactions += {
      case ButtonClicked(`startButton`) => {
        if (!platform.started) {
          platform.startup 
        }
        else {
          Dialog.showMessage(panel, "already started, please stop first")
        }
      }
      case ButtonClicked(`stopButton`) => platform.shutdown
    }
  }
  
  def startup(facade: Facade) {
    platform = facade
    SwingUtilities.invokeLater {
      new Runnable { 
        def run() { 
          top.pack()
          top.visible = true
        }
      }
    }
  }
}
