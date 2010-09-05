package com.harvey.domain

import com.harvey.service.sound.Signal

import scala.actors.Actor

sealed abstract class Message 
case class StopMessage() extends Message
case class ReadyMessage() extends Message
case class GenerateMessage() extends Message
case class TransmitMessage() extends Message
case class SignalMessage(signal: Signal) extends Message
case class BroadcastMessage(message: Message) extends Message
case class SignedMessage(sender: Actor, message: Message) extends Message