var HOST = document.location&&document.location.hostname||'intertwingly.net';

// Copyright (c) 1996-1997 Athenia Associates.
// http://www.webreference.com/js/
// License is granted if and only if this entire
// copyright notice is included. By Tomer Shiran.

function setCookie (name, value, expires, path, domain, secure) {
    var curCookie = name + "=" + escape(value) + ((expires) ? "; expires=" + expires.toGMTString() : "") + ((path) ? "; path=" + path : "") + ((domain) ? "; domain=" + domain : "") + ((secure) ? "; secure" : "");
    document.cookie = curCookie;
}

function getCookie (name) {
    var prefix = name + '=';
    var c = document.cookie;
    var nullstring = '';
    var cookieStartIndex = c.indexOf(prefix);
    if (cookieStartIndex == -1)
	return nullstring;
    var cookieEndIndex = c.indexOf(";", cookieStartIndex + prefix.length);
    if (cookieEndIndex == -1)
	cookieEndIndex = c.length;
    return unescape(c.substring(cookieStartIndex + prefix.length, cookieEndIndex));
}

function deleteCookie (name, path, domain) {
    if (getCookie(name))
	document.cookie = name + "=" + ((path) ? "; path=" + path : "") + ((domain) ? "; domain=" + domain : "") + "; expires=Thu, 01-Jan-70 00:00:01 GMT";
}

function fixDate (date) {
    var base = new Date(0);
    var skew = base.getTime();
    if (skew > 0)
	date.setTime(date.getTime() - skew);
}

function rememberMe (f) {
    var now = new Date();
    fixDate(now);
    now.setTime(now.getTime() + 365 * 24 * 60 * 60 * 1000);
    setCookie('cmt_name', f.name.value, now, '/', HOST, '');
    setCookie('cmt_email', f.email.value, now, '/', HOST, '');
    setCookie('cmt_url', f.url.value, now, '/', HOST, '');
}

function forgetMe (f) {
    deleteCookie('cmt_name', '/', HOST);
    deleteCookie('cmt_email', '/', HOST);
    deleteCookie('cmt_url', '/', HOST);
    f.name.value = '';
    f.email.value = '';
    f.url.value = '';
    f.bakecookie.checked = 0;
}

function initCommentForm(f) {
    if (!f.email.value) f.email.value = getCookie("cmt_email");
    if (!f.name.value) f.name.value = getCookie("cmt_name");
    if (!f.url.value) f.url.value = getCookie("cmt_url");
    if (getCookie("cmt_name") && f.bakecookie) f.bakecookie.checked = 1;
    if (getCookie("openid") && !getCookie("xmpp_id")) {
        var register = document.getElementById("navbar_register");
        if (register) {
            var color = 255;
            function fade() {
                register.style.color = "rgb(" + color.toString() + ",68,153)";
                if (color) {
                   register.style.fontWeight = "bold";
                   color-=15;
                   setTimeout(fade,1000);
                } else {
                   register.style.fontWeight = "normal";
                }
            }
            setTimeout(fade,2000);
        }
    }
}

//------------------------------------------------------------------------

function ns(prefix) {
  if (prefix == 'xhtml') return 'http://www.w3.org/1999/xhtml';
  return null;
} 
  
function xpath1(node, path) {
  if (!node.ownerDocument && node.documentElement) node=node.documentElement;
  return node.ownerDocument.evaluate(path, node, ns,
    XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;
}
    
function xpath(node, path) {
  if (!node.ownerDocument && node.documentElement) node=node.documentElement;
  return node.ownerDocument.evaluate(path, node, ns,
    XPathResult.ORDERED_NODE_ITERATOR_TYPE, null);
} 

function disable_submit(event) {
  event.target.disabled = true;
  return true; 
}

function autopreview() {
  comment = document.getElementById('comment');
  if (comment) {
    comment.onkeypress = function() {
      comment.onkeypress = undefined;
      if (document.getElementById('previewarea')) return;

      var xhr =  new XMLHttpRequest();
      xhr.open("POST", document.location, true);
      xhr.onreadystatechange = function() {
        if (xhr.readyState != 4) return;

        var button = xpath1(document, '//xhtml:input[@name="preview"]');
        var form = button;
        while (form && form.nodeName != 'form') form=form.parentNode;

        var warnings = xpath(xhr.responseXML, '//xhtml:div[@class="warning"]');
        var first = true;
        for (var node=warnings.iterateNext(); node; node=warnings.iterateNext()) {
          if (form) {
            node = form.ownerDocument.importNode(node, true);
            form.parentNode.insertBefore(node, form);
            if (first) scroll(0,node.offsetTop);
            first = false;
          }
        }

        var name = xpath1(document, '//xhtml:input[@name="name"]');
        if (!name.value) {
          var input = xpath1(xhr.responseXML, '//xhtml:input[@name="name"]');
          name.value = input.getAttribute('value');
        }

        var nonce = xpath1(xhr.responseXML, '//xhtml:input[@name="nonce"]');
        if (nonce) {
          nonce = document.importNode(nonce, true);
          button.parentNode.appendChild(nonce);
        }

        var captcha = xpath1(xhr.responseXML, '//xhtml:input[@id="captcha"]');
        if (captcha) {
          if (!captcha.getAttribute('value')) {
            var image = xpath1(xhr.responseXML, 
              '//xhtml:span[xhtml:input[@id="captcha"]]/xhtml:img[@alt]');
            captcha.setAttribute('value', image.getAttribute('alt'));
          }
          captcha.setAttribute('type', 'hidden');
          captcha = document.importNode(captcha, true);
          button.parentNode.appendChild(captcha);
        }

        /*
        TODO: Formatting Rules on main page?
        var rules = xpath1(xhr.responseXML,
          '//xhtml:form/following-sibling::xhtml:div[@class="blogbody"]');
        if (rules && form) {
          rules = document.importNode(rules, true);
          form.parentNode.insertBefore(rules,form);
        }
       */

        var livePreview = xpath1(xhr.responseXML,
          '//xhtml:input[@name="livePreview"]');
        if (livePreview) {
          livePreview = document.importNode(livePreview, true);
          var space = document.createTextNode('\n');
          button.parentNode.appendChild(space);
          button.parentNode.appendChild(livePreview);
          if (form) {
            var preview = document.createElementNS(ns('xhtml'),'div');
            preview.setAttribute('class','livepreview');
            preview.style.display="none";
            var h3 = document.createElementNS(ns('xhtml'),'h3');
            h3.appendChild(document.createTextNode('Live Preview'));
            preview.appendChild(h3);
            var div = document.createElementNS(ns('xhtml'),'div');
            div.setAttribute('id','previewarea');
            preview.appendChild(div);
            form.parentNode.insertBefore(preview,form.nextSibling);
            init_live_preview();
          }
        }

        var label = xpath1(xhr.responseXML,
          '//xhtml:label[@for="livePreview"]');
        if (label) {
          label = document.importNode(label, true);
          var space = document.createTextNode('\n');
          button.parentNode.appendChild(space);
          button.parentNode.appendChild(label);
        }

        if (!xpath1(document, '//xhtml:div[@class="warning"]')) {
          var submit = xpath1(xhr.responseXML, '//xhtml:input[@name="Submit"]');
          if (submit) {
            submit = document.importNode(submit, true);
            // submit.onclick = disable_submit;
            var space = document.createTextNode('\n');
            button.parentNode.insertBefore(submit, button);
            button.parentNode.insertBefore(space, button);
          }
        }
      }

      var query = "comment=autopreview&preview=1";
      var name = xpath1(document, '//xhtml:input[@name="name"]');
      if (name && name.value) query += '&name='+encodeURIComponent(name.value);
      var email = xpath1(document, '//xhtml:input[@name="email"]');
      if (email && email.value) query += '&email='+encodeURIComponent(email.value);
      var url = xpath1(document, '//xhtml:input[@name="url"]');
      if (url && url.value) query += '&url='+encodeURIComponent(url.value);
      xhr.setRequestHeader('Content-Type','application/x-www-form-urlencoded');
      xhr.send(query);

      init_update_watch();
    };
  }
}

if (document.addEventListener) {
  document.addEventListener("DOMContentLoaded", autopreview, false);
}

//------------------------------------------------------------------------

String.prototype.xescape = function() {
  return this.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

String.prototype.strip = function() {
  return this.replace(/^\s+/, '').replace(/\s+$/, '');
}

// create (or reset) a cookie
function createCookie(name,value,days) {
  if (days) {
    var date = new Date();
    date.setTime(date.getTime()+(days*24*60*60*1000));
    var expires = "; expires="+date.toGMTString();
  }
  else expires = "";
  document.cookie = name+"="+value+expires+"; path=/";
}
 
// read a cookie
function readCookie(name) {
  var nameEQ = name + "=";
  var ca = document.cookie.split(';');
  for(var i=0;i < ca.length;i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1,c.length);
    if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
  }
  return null;
}

var cp1252 = {
  128: 8364, // euro sign
  130: 8218, // single low-9 quotation mark
  131:  402, // latin small letter f with hook
  132: 8222, // double low-9 quotation mark
  133: 8230, // horizontal ellipsis
  134: 8224, // dagger
  135: 8225, // double dagger
  136:  710, // modifier letter circumflex accent
  137: 8240, // per mille sign
  138:  352, // latin capital letter s with caron
  139: 8249, // single left-pointing angle quotation mark
  140:  338, // latin capital ligature oe
  142:  381, // latin capital letter z with caron
  145: 8216, // left single quotation mark
  146: 8217, // right single quotation mark
  147: 8220, // left double quotation mark
  148: 8221, // right double quotation mark
  149: 8226, // bullet
  150: 8211, // en dash
  151: 8212, // em dash
  152:  732, // small tilde
  153: 8482, // trade mark sign
  154:  353, // latin small letter s with caron
  155: 8250, // single right-pointing angle quotation mark
  156:  339, // latin small ligature oe
  158:  382, // latin small letter z with caron
  159:  376  // latin capital letter y with diaeresis
};

function sanitize(body) {
  var hyperlink = function(str, href, text) {
    href = href.replace(/&amp;amp;/g, '&amp;');
    return '<a href="' + href + '">' + text + '</a>';
  }

  // code (literal) support
  var chunks = body.split(/\{\{\{([\s\S]*?)\}\}\}/);
  if (chunks.length > 1) {
    var work = chunks.slice();
    for (var i=1; i<chunks.length; i+=2) {
      if (chunks[i].indexOf('\n') >= 0) {
        chunks[i] = '<pre class="code">' + chunks[i].strip().xescape() + '</pre>';
        work[i] = '\n\n{{{' + i + '}}}\n\n';
      } else {
        chunks[i] = '<code>' + chunks[i].xescape() + '</code>';
        work[i] = '{{{' + i + '}}}';
      }
    }
    body = work.join('');
  } else {
    // naked urls become hypertext links
    var before = body;
    body = body.replace(/(^|[\s.:;?\-\]<\(])(http:\/\/[-\w;\/?:@&=+$.!~*\'()%,#]+[\w\/])(?=$|[\s.:;?\-\[\]>\)])/, '$1<a href="$2">[link]</a>');

  }

  // html characters used in text become escaped
  body = body.xescape();

  // canonicalize line control characters
  body = body.strip().replace(/\r\n?/g, '\n');

  // support NCR characters
  body=body.replace(/&amp;#(x[\da-zA-Z]+|\d+);/g, function(str, ncr) {
    if (ncr.charAt(0)=='x') {
      n = parseInt(ncr.substr(1),16);
    } else {
      n = parseInt(ncr);
    }

    if (n < 65536) {
      return String.fromCharCode(n);
    } else if (n < 0x10FFFF) {
      n = n - 0x10000;
      return String.fromCharCode(0xd800 + n/0x400) + 
             String.fromCharCode(0xdc00 + n%0x400);
    } else {
      return '&amp;#' + ncr + ';'
    }
  });

  // map windows-1252 to unicode
  for (var i=body.length-1; i>0; i--) {
    if (cp1252[body.charCodeAt(i)]) {
      body = body.substr(0,i) + '&#' + cp1252[body.charCodeAt(i)] + ';' + body.substr(i+1);
    }
  }
  
  // remove control characters
  body = body.replace(/[\x00-\x08\x0B\x0C\x0E-\x1F]/g, function(str) {
    var code = "000" + str.charCodeAt(0).toString(16).toUpperCase();
    return '<acronym title="U+' + code.substr(code.length-4) + '">&#xFFFD;</acronym>'; 
  });

  // passthru <a href>, <em>, <i>, <b>, <blockquote>, <br/>, <p>
  body=body.replace(/&lt;a href="([^"]*)"&gt;([^&]*)&lt;\/a&gt;/g, hyperlink);
  body=body.replace(/&lt;a href='([^']*)'&gt;([^&]*)&lt;\/a&gt;/g, hyperlink);
  body=body.replace(/&lt;abbr title="([^"]*)"&gt;([^&]*)&lt;\/abbr&gt;/,
            '<abbr title="$1">$2</abbr>');
  body=body.replace(/&lt;acronym title="([^"]*)"&gt;([^&]*)&lt;\/acronym&gt;/g,
            '<acronym title="$1">$2<\/acronym>');
  body=body.replace(/&lt;em&gt;([^&]*)&lt;\/em&gt;/g, '<em>$1<\/em>');
  body=body.replace(/&lt;i&gt;([^&]*)&lt;\/i&gt;/g, '<i>$1<\/i>');
  body=body.replace(/&lt;b&gt;([^&]*)&lt;\/b&gt;/g, '<b>$1<\/b>');
  body=body.replace(/&lt;strong&gt;([^&]*)&lt;\/strong&gt;/g, '<strong>$1<\/strong>');
  body=body.replace(/&lt;blockquote&gt;([^~]*?)&lt;\/blockquote&gt;/g,
            '\n\n<blockquote><p>$1<\/p><\/blockquote>\n\n');
  body=body.replace(/&lt;br\s*\/?&gt;\n?/g, '\n');
  body=body.replace(/&lt;\/?p&gt;/g, '\n\n').strip();

  // typographic support for mdash, curly quotes
  body=body.replace(/(\s)--(\s)/g, '$1&#8212;$2');
  body=body.replace(/(\w)\'(\w)/g, '$1&#8217;$2');
  body=body.replace(/(^|\s)'([^<]*?)'(\s|[,;.\]]|$)/g, '$1&#8216;$2&#8217;$3');
  body=body.replace(/(^|\s)"([^<]*?)"(\s|[,;.\]]|$)/g, '$1&#8220;$2&#8221;$3');

  // wiki like support: _em_, -del-, *b*, [url title]
  body=body.replace(/\b_(\w.*?)_\b/g, '<em>$1</em>');
  body=body.replace(/\*(\w.*?\w)\*/g, '<b>$1</b>');
  body=body.replace(/\[(\w+:\S+\.gif) (.*?)\]/g, '<img src="$1" alt="$2" />');
  body=body.replace(/\[(\w+:\S+\.jpg) (.*?)\]/g, '<img src="$1" alt="$2" />');
  body=body.replace(/\[(\w+:\S+\.png) (.*?)\]/g, '<img src="$1" alt="$2" />');
  body=body.replace(/\[(\w+:\S+) (.*?)\]/g, hyperlink).strip();

  // cvs urls
  body=body.replace(/\/<b>checkout<\/b>\//g, '/*checkout*/')
  body=body.replace(/\/<em>checkout<\/em>\//g, '/_checkout_/')

  // email style quotes (lines beginning with '>')
  body=body.replace(/^&gt;(.*)\n\n+&gt;/g, '&gt;$1\n&gt;\n&gt;');
  var quotes=body.split(/^( *&gt;.*(?:\n *&gt;.*)*)/m);
  for (var i=1; i<quotes.length; i+=2) {
    var html = '';
    var depth = 0;
    var qline=/^((?: |&gt;)*)(.*)/gm;
    for (var match; match=qline.exec(quotes[i]);) {
      var indent=match[1].replace(/ /g,'').replace(/&gt;/g,'>').length;
      while (indent>depth) depth++, html+='<blockquote class="quote"><p>';
      while (indent<depth) depth--, html+='</p></blockquote>';
      html+=match[2]+"\r";
    }
    while (depth>0) depth--, html+='</p></blockquote>';
    quotes[i]=html;
  }
  body=quotes.join('');

  // unordered lists: consecutive lines starting with spaces and an asterisk
  var lists=body.split(/^( *\*.*(?:\n *\*.*)*)\n*/m);
  for (var i=1; i<lists.length; i+=2) {
    var html = '';
    var stack = [''];
    var li = /( +)\* +(.*)/g;
    for (var match; match=li.exec(lists[i]);) {
      var indent=match[1];
      if (indent>stack[stack.length-1]) stack=stack.concat(indent), html+='\n<ul>\r';
      while (indent<stack[stack.length-1]) stack.pop(), html+='</ul>\r';
      if (match[2]) html += '<li>'+match[2]+'</li>\r';
    }
    while (stack.length > 1) stack.pop(), html+='</ul>\r';
    lists[i]=html+"\n\n";
  }
  body=lists.join('');

  // white space
  var stanzas=body.strip().split(/\n\n+/);
  if (stanzas.length>1) body='<p>' + stanzas.join('</p>\r<p>') + '</p>\r';
  if (stanzas.length>1) {
    for (var i=0; i<stanzas.length; i++) {
      if (stanzas[i].substr(0,4) != '<ul>' && stanzas[i].substr(0,11) != '<blockquote') {
        stanzas[i] = '<p>' + stanzas[i] + '</p>'
      }
    }
    body=stanzas.join('\r');
  }
  body=body.replace(/\n<\/p>/g, '</p>');
  body=body.replace(/\n/g, '<br />\n');
  body=body.replace(/  +/g, '&#160; ');

  // reinsert literals
  if (chunks.length > 1) {
    for (var i=1; i<chunks.length; i++) {
      body=body.replace('<p>{{{'+i+'}}}</p>', chunks[i]);
      body=body.replace('{{{'+i+'}}}', chunks[i]);
    }
  }

  // renormalize linefeeds
  body=body.replace(/\r\n*/g, '\n').strip();

  return body;
}

prev_value = '';

function live_preview() {
  var checkbox = document.getElementById('livePreview');
  if (!checkbox.checked) return;

  var preview = document.getElementById('previewarea');
  if (preview && preview.parentNode.getAttribute('class')=='livepreview') {
    preview.parentNode.style.display='block';
  }

  var input = document.getElementById('comment');
  if (input.value != prev_value) {

    try {
      preview.innerHTML = sanitize(input.value);
    } catch(err) {
      preview.innerHTML = input.value.xescape();
    }

    prev_value = input.value;
  }
  setTimeout(live_preview,500);
}

function init_live_preview() {
  var checkbox = document.getElementById('livePreview');
  if (checkbox) {
    var cookie = readCookie("livePreview");
    if (cookie && cookie == 'true') {
      checkbox.checked = true;
      live_preview();
    }
    checkbox.onclick = function() {
      var checkbox = document.getElementById('livePreview');
      if (!checkbox) return;
      createCookie("livePreview", checkbox.checked?'true':'false', 365);
      if (checkbox.checked) {
        live_preview();
      } else {
        var preview = document.getElementById('previewarea');
        if (preview) {
          if (preview.parentNode.getAttribute('class')=='livepreview') {
            preview.parentNode.style.display='none';
          }
        }
      }
    }
  }
}

if (document.addEventListener) {
    document.addEventListener("DOMContentLoaded", init_live_preview, false);
}

//------------------------------------------------------------------------

function commentIterator(doc, f) {
  var warnings = xpath(doc, '//xhtml:a[@id] | //xhtml:h2[text()]');
  for (var node=warnings.iterateNext(); node; node=warnings.iterateNext()) {
    f(node);
  }
}

var visible = {};
function init_update_watch() {
  commentIterator(document, function(node) {
    var value;
    if (node.nodeName == 'a') value = node.getAttribute('id');
    if (node.nodeName == 'h2') value = node.childNodes[0].nodeValue;
    if (value) visible[value] = 1;
  });
  update_watch();
}

function add_comments() {
  var xhr =  new XMLHttpRequest();
  xhr.open("GET", document.location, true);
  xhr.onreadystatechange = function() {
    if (xhr.readyState != 4) return;
    var form = xpath1(document, '//xhtml:div[@class="commentform"]');
    new_comments = 0;
    commentIterator(xhr.responseXML, function(node) {
      if (node.nodeName == 'a') {
        var value = node.getAttribute('id');
        if (value && !visible[value]) {
          node = document.importNode(node.parentNode, true);
          form.parentNode.insertBefore(node, form);
          if (!new_comments) scroll(0,node.offsetTop);
          new_comments++;
        }
      } else if (node.nodeName == 'h2') {
        var value = node.childNodes[0].nodeValue;
        if (value && !visible[value]) {
          node = document.importNode(node, true);
          form.parentNode.insertBefore(node, form);
        }
      }
    });
    if (new_comments == 1) alert('1 new comment');
    if (new_comments > 1) alert(new_comments + ' new comments');
    init_update_watch();
  }
  xhr.send(null);

  var refresh = xpath1(document, '//xhtml:input[@value="Refresh"]');
  refresh.parentNode.removeChild(refresh);
  return false;
}

function update_watch() {
  var xhr =  new XMLHttpRequest();
  xhr.open("GET", document.location, true);
  xhr.onreadystatechange = function() {
    if (xhr.readyState != 4) return;

    var updated=false;;
    commentIterator(xhr.responseXML, function(node) {
      var value;
      if (node.nodeName == 'a') value = node.getAttribute('id');
      if (node.nodeName == 'h2') value = node.childNodes[0].nodeValue;
      if (value && !visible[value]) updated=true;
    });

    if (updated) {
      var button = xpath1(document, '//xhtml:input[@name="preview"]');
      refresh = document.createElementNS(ns('xhtml'),'input');
      refresh.setAttribute('type','submit');
      refresh.setAttribute('value','Refresh');
      refresh.setAttribute('title','See new comments');
      refresh.style.backgroundColor = '#F00';
      refresh.style.color = '#FFF';
      refresh.onclick=add_comments;
      button.parentNode.insertBefore(refresh, button);
      button.parentNode.insertBefore(document.createTextNode('\n'), button);
    } else {
      setTimeout(update_watch,30000);
    }

    var submit = xpath1(document, '//xhtml:input[@name="Submit"]');
    if (submit) submit.disabled = false;
  }
  xhr.send(null);
}
