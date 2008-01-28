// String extensions
jQuery.extend(String.prototype, {
	Left: function(characterCount)
	{
		return this.substring(0,characterCount);
	},
	
	// Right
	Right: function(characterCount)
	{
		return this.substring((this.length - characterCount), this.length);
	},
	
	// Mid
	Mid: function(start, end)
	{
		if(!start){start=0};
		if(!end || end > this.length){end=this.length};
		if(end != this.length){end = start + end};
		return this.substring(start,end);
	},
	
	// Replace
	Replace: function(replaceThis, replaceWith)
	{
		var retval = this;
		while(retval.indexOf(replaceThis) > -1){
			retval = retval.replace(replaceThis,replaceWith);
		}
		return retval;
	},
	
	// Trim
	Trim: function()
	{
		var retval = this;
		retval = this.TrimStart();
		return retval.TrimEnd();
	},
	
	// TrimStart
	TrimStart: function ()
	{
		var retval = this;
		while(retval.charAt(0)==" "){
			retval = retval.replace(retval.charAt(0),"");
		}
		return retval;
	},
	
	// TrimEnd
	TrimEnd: function ()
	{
		var retval = this;
		while(retval.charAt((retval.length -1))==" "){
			retval = retval.substring(0,retval.length-1);
		}
		return retval;
	},
	
	// RemoveAllWhitespace
	RemoveAllWhitespace: function ()
	{
		var exp = new RegExp('\\s{1,}', 'gi');
		return this.replace(exp, '');
	}
	
});


// Returns a value of a field as a string.
// Will return checkbox lists etc as a comma separated string
jQuery.fn.fieldStringVal = function(successful) {
	var retval = new String();
	if (this.length > 0){
		var elm = this[0];
		var name = new String(elm.name);
		var t = elm.type;
		if (t == "checkbox" || t == "radio"){
			if (name == ""){
				if (elm.checked) retval = elm.id;
			}
			else{
				/* See if it's a checkbox / radiobutton list */
				/* Find all controls of the same name */
				jQuery("input[@name=" + name + "]").each(function(){
					if (this.checked){
						if (retval != "") retval += ",";
						retval += this.id;
					}
				});
			}
		}
		else if (t == "select-one" || t == "select-multiple"){
			for (var i=0; i<elm.length; i++){
				if (elm.options[i].selected){
					if (retval != "") retval += ",";
					retval += elm.options[i].value;
				}
			}
		}
		else{
			retval = elm.value;
		}
	}
	return jQuery.trim(retval);
};

