# Widget.tcl - a set of useful widgets for Wub

package require RAM
package require Form

package provide Widget 1.0

RAM init Widget /widget/

Widget set login.stx "" content-type x-text/stx
    
Widget set login.html [<form> login \
			   action "" \
			   method post \
			   {
			       [<div> id username [subst {
				   [<label> for username-field class overlabel Username]
				   [<text> username id username-field title Username tabindex 1 ""]
			       }]]

			       [<div> id password [subst {
				   [<label> for password-field class overlabel Password]
				   [<password> password id password-field title Password tabindex 2 ""]
			       }]]

			       [<submit> submit tabindex 3 Login]
			       [<script> type text/javascript src /widget/login.js ""]
			   }] content-type x-text/html-fragment -headers [list [<style> type text/css {@import url(/widget/login.css);}]]

Widget set login.js {
    function initOverLabels () {
	if (!document.getElementById) return;      
	
	var labels, id, field;

	// Set focus and blur handlers to hide and show 
	// labels with 'overlabel' class names.
	labels = document.getElementsByTagName('label');
	for (var i = 0; i < labels.length; i++) {
             if (labels[i].className == 'overlabel') {
		 // Skip labels that do not have a named association
		 // with another field.
		 id = labels[i].htmlFor || labels[i].getAttribute ('for');
		 if (!id || !(field = document.getElementById(id))) {
		     continue;
		 }

		 // Change the applied class to hover the label 
		 // over the form field.
		 labels[i].className = 'overlabel-apply';

		 // Hide any fields having an initial value.
		 if (field.value !== '') {
		     hideLabel(field.getAttribute('id'), true);
		 }

		 // Set handlers to show and hide labels.
		 field.onfocus = function () {
		     hideLabel(this.getAttribute('id'), true);
		 };
		 field.onblur = function () {
		     if (this.value === '') {
			 hideLabel(this.getAttribute('id'), false);
		     }
		 };

		 // Handle clicks to label elements (for Safari).
		 labels[i].onclick = function () {
		     var id, field;
		     id = this.getAttribute('for');
		     if (id && (field = document.getElementById(id))) {
			 field.focus();
		     }
		 };
	     }
	 }
    };

    function hideLabel (field_id, hide) {
	var field_for;
	var labels = document.getElementsByTagName('label');
	for (var i = 0; i < labels.length; i++) {
            field_for = labels[i].htmlFor || labels[i].getAttribute('for');
	    if (field_for == field_id) {
		labels[i].style.textIndent = (hide) ? '-1000px' : '0px';
		return true;
	    }
	}
    };

    window.onload = function () {
	setTimeout(initOverLabels, 50);
    };
}  content-type text/javascript

Widget set login.css "
form#login {
  position:relative;
}

div#username,
div#password {
  position:relative;
  float:left;
  margin-right:3px;
}

input#username-field,
input#password-field {
  width:10em;
}

  label.overlabel {
    color:#999;
  }

  label.overlabel-apply {
    position:absolute;
    top:3px;
    left:5px;
    z-index:1;
    color:#999;
  }
" content-type text/css
