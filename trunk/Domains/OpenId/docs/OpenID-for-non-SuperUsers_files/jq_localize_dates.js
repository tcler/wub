// Parse an HTML5-liberalized version of RFC 3339 datetime values
Date.parseRFC3339 = function (string) {
    var date=new Date(0);
    var match = string.match(/(\d{4})-(\d\d)-(\d\d)\s*(?:[\sT]\s*(\d\d):(\d\d)(?::(\d\d))?(\.\d*)?\s*(Z|([-+])(\d\d):(\d\d))?)?/);
    if (!match) return;
    if (match[2]) match[2]--;
    if (match[7]) match[7] = (match[7]+'000').substring(1,4);
    var field = [null,'FullYear','Month','Date','Hours','Minutes','Seconds','Milliseconds'];
    for (var i=1; i<=7; i++) if (match[i]) date['setUTC'+field[i]](match[i]);
    if (match[9]) date.setTime(date.getTime()+
        (match[9]=='-'?1:-1)*(match[10]*3600000+match[11]*60000) );
    return date.getTime();
}

// Localize the display of <time> elements
function localizeDates() {
  var sections = $('section');
  var lastdate = '';
  var now = new Date();

  $('time').each (function() {
    var time = $(this);

    if (time.attr('title') == "GMT") {
      var date = new Date(Date.parseRFC3339(time.attr('datetime')));
      if (!date.getTime()) return;

      // replace title attribute and text value with localized versions
      if (time.attr('datetime').length <= 16) {
        // date only
        time.removeAttr('title');
        time.text(date.toLocaleDateString());
      } else if (time.text().length < 10 || now-date < 86400000) {
        // time only
        time.attr('title', date.toUTCString());
        time.text(date.toLocaleTimeString());
      } else {
        // full datetime
        time.attr('title', time.textContent + ' GMT');
        time.text(date.toLocaleString());
      }

      // Make webkit time zone information more compact
      time.text(time.text().replace(/ GMT(-\d\d\d\d) \(.*\)$/, ''));

      // insert/remove date headers to reflect date in local time zone
      time.parents('article').each(function() {
        if (this.parentNode.tagName.toLowerCase() == 'section') {
          var displayDate = date.toLocaleDateString();
          if (displayDate != lastdate) {
            var datetime = time.attr('datetime').substring(0,10);
            datetime = '<time datetime="'+datetime+'">'+displayDate+'</time>';
            var header = '<header><h2>'+datetime+'</h2></header>'
            section = $('<section>'+header+'</section>');
            lastdate = displayDate;
            section.insertBefore(sections[0]);
            $('<hr/>').insertBefore(sections[0]);
          } else {
            section.append("\n\n<hr/>\n\n");
          }
          section.append(this);
        }
      });
    }
  });

  // remove original sections
  if (lastdate != '') sections.not(':has(form)').remove();
}

$(document).ready(function() {
  localizeDates();
})

// allow IE to recognize HTMl5 elements
if (!document.createElementNS) {
  document.createElement('article');
  document.createElement('section');
  document.createElement('aside');
  document.createElement('footer');
  document.createElement('header');
  document.createElement('nav');
  document.createElement('time');
}
