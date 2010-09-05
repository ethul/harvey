package com.harvey.service.sound

sealed abstract class SignalContext {
  def intervals: List[Int]
  def lengths: List[Int]
  def maximumAmplitude: Int
  def maximumPatternLength: Int
  def maximumFrequency: Int
  def minimumFrequency: Int
  def lowFrequencySources: List[Int]
  def zeroLengthSources: List[Int]
  def normalFrequencySources: List[Int]
}

object MidiSignalContext extends SignalContext {
  //def intervals = List(-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12)
  def intervals = List(-12,-10,-8,-6,-4,-2,0,2,4,6,8,10,12)
  def lengths = List(200,300,400,500,600,700,800,900,1000,1500)  
  def maximumAmplitude = 55
  def maximumPatternLength = 10
  def maximumFrequency = 110
  def minimumFrequency = 20
  def lowFrequencySources = List(32,33,34,35,36,37,38,39,43,62,63,67,76)
  def zeroLengthSources = List(112,113,114,115,116,117,118,119)
  def normalFrequencySources = (0 to 127).toList.filter(a => !(lowFrequencySources.contains(a) || zeroLengthSources.contains(a)))
}
