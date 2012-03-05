var Harvey = (function(App) {
  App.FrequencyView = Ember.View.extend({
  });

  App.mainController = Ember.Object.create({
    frequency: undefined,
    compose: function() {
      var dev = audioLib.AudioDevice(audioCallback,2)
        , keyNotes = Note.fromLatin("C4").scale("major")
        , key = keyNotes[Math.floor(Math.random() * keyNotes.length)]
        , frequencies = _.map(_.union(
              Note.fromLatin(key.latin() + "4").scale("harmonic minor")
            , Note.fromLatin(key.latin() + "5").scale("harmonic minor")
          ),function(a){return a.frequency();})
        , osc = audioLib.Oscillator(dev.sampleRate)
        , envelope = audioLib.ADSREnvelope(dev.sampleRate)
        , comp = audioLib.Compressor(dev.sampleRate,3)
        , durations = _.range(1,5)
        , current = 0
        , that = this;

      envelope.triggerGate();

      function audioCallback(buffer, channelCount){
        var n = buffer.length
          , k = channelCount
          , i = 0
          , j = 0
          , sample = undefined;

        if (current < 1) {
          osc.frequency = frequencies[Math.floor(Math.random() * frequencies.length)];
          current = durations[Math.floor(Math.random() * durations.length)];
          that.set("frequency",osc.frequency);
        }
        
        current--;

        for (i = 0; i < n; i += k) {
          envelope.generate();
          osc.generate();
          sample = envelope.getMix() * osc.getMix();
          for (j = 0; j < k; j++) {
            buffer[i+j] = sample;
          }
        }
      }
    },
  });
  return App;
}(Harvey || Ember.Application.create()));
