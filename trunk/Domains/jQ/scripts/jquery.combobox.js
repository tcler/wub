/* 
   http://jqueryui.com/demos/autocomplete/#combobox

   $(function() {
   	$("#combobox").combobox();
	$("#toggle").click(function() {
		$("#combobox").toggle();
	});
   });
*/

(function( $ ) {
    $.widget("ui.combobox", {
	    _create: function() {
		var self = this;
		var select = this.element.hide(),
		    selected = select.children( ":selected" ),
		    value = selected.val() ? selected.text() : "";
		var input = $("<input>")
		    .insertAfter(select)
		    .val(value)
		    .autocomplete({
			    delay: 0,
			    minLength: 0,
			    source: function(request, response) {
				var matcher = new RegExp( $.ui.autocomplete.escapeRegex(request.term), "i" );
				response(select.children("option").map(function() {
					    var text = $(this).text();
					    if (this.value && (!request.term || matcher.test(text)))
						return {
						    label: text.replace(
									new RegExp(
										   "(?![^&;]+;)(?!<[^<>]*)(" +
										   $.ui.autocomplete.escapeRegex(request.term) +
										   ")(?![^<>]*>)(?![^&;]+;)", "gi"
										   ), "<strong>$1</strong>" ),
							value: text,
							option: this
							};
					}) );
			    },

			    select: function(event, ui) {
				ui.item.option.selected = true;
				//select.val( ui.item.option.value );
				self._trigger("selected", event, {
					item: ui.item.option
					    });
			    },

			    change: function(event, ui) {
				if (!ui.item) {
				    var matcher = new RegExp("^" + $.ui.autocomplete.escapeRegex($(this).val()) + "$", "i"),
				    valid = false;
				    select.children("option").each(function() {
					    if (this.value.match(matcher)) {
						this.selected = valid = true;
						return false;
					    }
					});
				    if (!valid) {
					// remove invalid value, as it didn't match anything
					$(this).val("");
					select.val("");
					return false;
				    }
				}
			    }
			})
		    .addClass("ui-widget ui-widget-content ui-corner-left");

		input.data("autocomplete")._renderItem = function(ul, item) {
		    return $("<li></li>")
		    .data("item.autocomplete", item)
		    .append("<a>" + item.label + "</a>")
		    .appendTo(ul);
		};

		$("<button>&nbsp;</button>")
		    .attr( "tabIndex", -1 )
		    .attr("title", "Show All Items")
		    .insertAfter(input)
		    .button({
			    icons: {primary: "ui-icon-triangle-1-s"},
				text: false
				})
		    .removeClass("ui-corner-all")
		    .addClass("ui-corner-right ui-button-icon")
		    .click(function() {
			    // close if already visible
			    if ( input.autocomplete("widget").is(":visible")) {
				input.autocomplete("close");
				return;
			    }

			    // pass empty string as value to search for, displaying all results
			    input.autocomplete("search", "");
			    input.focus();
			});
	    }
	});
})(jQuery);
