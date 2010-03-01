package com.harvey.sound

import javax.sound.midi.MidiSystem

/**
 * contract of a sound device
 * 
 * @author ethul
 * @date 2010.02.28
 * @email eric.thul AT gmail.com
 */
trait SoundDevice {
  def sinks(): List[SignalSink]
  def release(): Unit
}

/**
 * companion factory object of a sound device
 * 
 * <p>
 *   the sound device may be acquired by calling
 *   <code>SoundDevice acquire</code>
 * </p>
 * 
 * @author ethul
 * @date 2010.02.28
 * @email eric.thul AT gmail.com
 */
object SoundDevice {
  def apply(): SoundDevice = new MidiDeviceImpl
  
  private class MidiDeviceImpl extends SoundDevice {
    private val synthesizer = MidiSystem.getSynthesizer
    private val midiSinks = synthesizer.getChannels.filter(_ != null).map(new MidiSink(_)).toList
    
    synthesizer.open
    
    def sinks(): List[SignalSink] = midiSinks
    def release(): Unit = synthesizer.close 
    override def toString(): String = {
      "vendor: " + synthesizer.getDeviceInfo.getVendor + "\n" +
      "name: " + synthesizer.getDeviceInfo.getName + "\n" +
      "description: " + synthesizer.getDeviceInfo.getDescription + "\n" +
      "version: " + synthesizer.getDeviceInfo.getVersion + "\n" + 
      "number of transmitters: " + synthesizer.getMaxTransmitters + "\n" +
      "number of receivers: " + synthesizer.getMaxReceivers + "\n" + 
      "polyphony limit: " + synthesizer.getMaxPolyphony + "\n" +
      "processing latency in microseconds: " + synthesizer.getLatency + "\n" +
      "microsecond position: " + synthesizer.getMicrosecondPosition + "\n\n" +
      "available instruments:" + "\n" + synthesizer.getAvailableInstruments.mkString("\n")
    }
  }
}
