 //global vars
 var timer;
 var progTimer;
 var pauze = $("#stopstart");
 var next = $("#next");
 var prev = $("#prev");
 var progressbar = $("#progressbar")
 var progressLabel = $(".progress-label");
 var jsonData = JSON.parse($('#input_data').html());
 var len = jsonData.length;
 var total = 0;
 var handmatig = false;
 var curpos = 0;

 for (var x in jsonData) {
     total = total + jsonData[x].time;
 }
 var totalTime = (total + 200) / 100;

 $('#duration').html("Total Typing Duration: <b> " + total + "</b>");
 $('#output').html("");

 progressbar.progressbar({
     value: true,
     change: function() {
         progressLabel.text((progressbar.progressbar("value") || 0) + "%");
     },
     complete: function() {
         progressLabel.text("Complete!");
     }
 });

 function Timer(callback, delay) {
     var timerId, start, remaining = delay;

     this.pause = function() {
         window.clearTimeout(timerId);
         remaining -= new Date() - start;
     };

     this.resume = function() {
         start = new Date();
         window.clearTimeout(timerId);
         timerId = window.setTimeout(callback, remaining);
     };
     this.clear = function() {
         window.clearTimeout(timerId);
     };

     this.resume();
 }

 function replaceRange(s, start, end, substitute) {
     return s.substring(0, start) + substitute + s.substring(end);
 }

 function textLoop() {
     String.prototype.replaceAt = function(index, character) {
         return this.substr(0, index) + character + this.substr(index + character.length);
     }
     String.prototype.insert = function(index, string) {
         if (index > 0)
             return this.substring(0, index) + string + this.substring(index, this.length);
         else
             return string + this;
     };


     if (handmatig == true) { //next key

         var text = $('#output').text();
         var key = jsonData[curpos].key
         var pos = jsonData[curpos].pos
         insert = false;
         if (key == "%0D") { //nieuwe regel
             text = text.replaceAt(pos, "\n");
         } else if (key == "%20") { //spatie
             text = text.replaceAt(pos, " ");
         } else if (key == ".") { //delete knop
             text = text.replaceAt(pos, " ");
         } else if (key == "%08") { //backspace
             console.log(pos - 1);
             text = text.replaceAt(pos - 1, " ");
             //    insert = true;
         } else {
             text = text.insert(pos, key);
         }
         $('#output').html(text.replace(/ /g, ''));

     } else { //normal playing
         while (curpos < len) {
             var text = $('#output').text();
             var key = jsonData[curpos].key
             var pos = jsonData[curpos].pos
             insert = false;
             console.log(key + "  " + pos);
             if (key == "%0D") { //nieuwe regel
                 text = text.replaceAt(pos, "\n");
             }

              else if (key == "%20") { //spatie
                 text = text.replaceAt(pos, " ");
             } else if (key == ".") { //delete knop
                 text = text().replaceAt(pos, " ");
             } else if (key == "%08") { //backspace

                 text = text.replaceAt(pos - 1, " ");
             } else {
                 text = text.insert(pos, key);
             }
             text = decodeURIComponent(text);

             $('#output').html(text.replace(/\(.*?\)/g, function(string) {
                                                                   return string.replace(/\s/g, '');
                                                               }));
             curpos++;
             timer = new Timer(function() {
                 textLoop();
             }, jsonData[curpos].time);

             break;
         }
     }
 }


 function progress() {
     var val = progressbar.progressbar("value") || 0;
     progressbar.progressbar("value", val + 1);

     if (val < 99) {
         progTimer = new Timer(progress, totalTime);
     } else {
         next.attr("disabled", "true");
         pauze.attr("disabled", "true");
     }
 }

 function startPlaying() {

     curpos = 0;
     handmatig = false;
     pauze.removeAttr("disabled");
     next.removeAttr("disabled");
     prev.removeAttr("disabled");
     $('#output').empty();
     progressbar.progressbar("value", 0);
     if (typeof timer != "undefined") {
         timer.clear();
         progTimer.clear();
     }
     pauze.html('Pause Playing');
     textLoop();

     progTimer = new Timer(progress, totalTime);

 }

 function pausePlaying() {
     handmatig = false;
     if (pauze.html() == "Pause Playing") {
         pauze.html('Continue Playing');
         timer.pause()
         progTimer.pause();
         // alert("pause");
     } else {
         pauze.html('Pause Playing');
         timer.resume()
         progTimer.resume();
         // alert("pause");

     }
 }

 function nextKey() {
     if (typeof timer != "undefined") {
         timer.pause()
         progTimer.pause();
     }
     handmatig = true;
     pauze.html('Continue Playing');
     if (curpos < len) {
         prev.removeAttr("disabled");
         textLoop();
         var val = progressbar.progressbar("value");
         progressbar.progressbar("value", parseInt((curpos / len) * 100));

         curpos++;
     } else {
         progressbar.progressbar("value", 100);
         next.attr("disabled", "true");
         prev.removeAttr("disabled");
     }

 }

 function prevKey() {
     if (typeof timer != "undefined") {
         timer.pause()
         progTimer.pause();
     }
     handmatig = true;
     pauze.html('Continue Playing');
     pauze.removeAttr("disabled");
     if (curpos > 0) {
         next.removeAttr("disabled");
         var val = progressbar.progressbar("value");
         progressbar.progressbar("value", parseInt((curpos / len) * 100));
         var nieuwtekst = $('#output').text().slice(0, -1)
         $('#output').html(nieuwtekst);
         curpos--;
     } else {
         progressbar.progressbar("value", 0);
         prev.attr("disabled", "true");
         next.removeAttr("disabled");
     }


 }