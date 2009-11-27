(function($) {
	
	$.fn.stickynote = function(options) {
		var opts = $.extend({}, $.fn.stickynote.defaults, options);
		return this.each(function() {
			$this = $(this);
			var o = $.meta ? $.extend({}, opts, $this.data()) : opts;
			switch(o.event){
				case 'dblclick':
					$this.dblclick(function(e){$.fn.stickynote.createNote(o);})
					break;
				case 'click':
					$this.click(function(e){$.fn.stickynote.createNote(o);})
					break;
			}		
		});
	};
	$.fn.stickynote.defaults = {
		size 	: 'small',
		event	: 'click',
		color	: '#000000'
	};
	$.fn.stickynote.createNote = function(o) {
		var _note_content = $(document.createElement('textarea'));
		var _div_note 	= 	$(document.createElement('div'))
							.addClass('jStickyNote')
							.css('cursor','move');
		if (o.id) {
		    _div_note.attr('id', o.id);
		}

		if(!o.text){
			_div_note.append(_note_content);
			var _div_create = $(document.createElement('div'))
						.addClass('jSticky-create')
						.attr('title','Create Sticky Note');
		
			_div_create.click(function(e){
				var _content = $(this).parent().find('textarea').val();
				var _p_note_text = $(document.createElement('p'))
				    .css('color',o.color)
				    .html(_content);
				$(this)
				    .parent()
				    .find('textarea')
				    .before(_p_note_text)
				    .remove(); 
				
				$(this).remove();
				$.post('/sticky/save',"content="+_content);
			    });
		} else
			_div_note.append('<p style="color:'+o.color+'">'+o.text+'</p>');					
		
		var _div_delete = $(document.createElement('div')).addClass('jSticky-delete');
		if (o.id) {
		    _div_delete.attr('id', o.id);
		}
		
		_div_delete.click(function(e){
			var _id = $(this).attr('id');
			$(this).parent().remove();
			$.post('/sticky/delete',"id="+_id);
		})
		
		var _div_wrap 	= 	$(document.createElement('div'))
							.css({'position':'absolute','top':'0','left':'0'})
							.append(_div_note)
							.append(_div_delete)
							.append(_div_create);	
		switch(o.size){
			case 'large':
				_div_wrap.addClass('jSticky-large');
				break;
			case 'small':
				_div_wrap.addClass('jSticky-medium');
				break;
		}		
		if(o.containment){
			_div_wrap.draggable({ containment: '#'+o.containment , scroll: false ,start: function(event, ui) {
				if(o.ontop)
					$(this).parent().append($(this));
			}});	
		}	
		else{
			_div_wrap.draggable({ scroll: false ,start: function(event, ui) {
				if(o.ontop)
					$(this).parent().append($(this));
			}});	
		}

		if (o.container) {
		    $(o.container).append(_div_wrap);
		} else if (o.containment) {
		    $(o.containment).append(_div_wrap);
		} else {
		    $('#content').append(_div_wrap);
		}
	};
})(jQuery);
