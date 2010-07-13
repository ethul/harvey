package com.harvey.platform

import com.harvey.gui.GuiFacade

/**
 * this object is the entry point
 * 
 * @author ethul
 * @date 2010.02.22
 * @email eric.thul AT gmail.com
 */
object Harvey {
  /**
   * entry point for harvey
   */
  def main(args: Array[String]) {
    (new GuiFacade).startup(new Facade)
  }
}
