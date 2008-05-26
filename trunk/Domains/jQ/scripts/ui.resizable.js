/*
 * jQuery UI Resizable
 *
 * Copyright (c) 2008 Paul Bakaus
 * Dual licensed under the MIT (MIT-LICENSE.txt)
 * and GPL (GPL-LICENSE.txt) licenses.
 * 
 * http://docs.jquery.com/UI/Resizables
 *
 * Depends:
 *	ui.core.js
 *
 * Revision: $Id: ui.resizable.js 5453 2008-05-05 16:50:46Z rdworth $
 */
;(function($) {
	
	$.widget("ui.resizable", {
		init: function() {

			var self = this, o = this.options;
			
			
			var elpos = this.element.css('position'); // simulate .ui-resizable { position: relative; }
			this.element.addClass("ui-resizable").css({ position: /static/.test(elpos) ? 'relative' : elpos });
			
			$.extend(o, {
				_aspectRatio: !!(o.aspectRatio),
				proxy: o.proxy || o.ghost || o.animate ? 'proxy' : null,
				knobHandles: o.knobHandles === true ? 'ui-resizable-knob-handle' : o.knobHandles
			});

			//Default Theme
			var aBorder = '1px solid #DEDEDE';
	
			o.defaultTheme = {
				'ui-resizable': { display: 'block' },
				'ui-resizable-handle': { position: 'absolute', background: '#F2F2F2', fontSize: '0.1px' },
				'ui-resizable-n': { cursor: 'n-resize', height: '4px', left: '0px', right: '0px', borderTop: aBorder },
				'ui-resizable-s': { cursor: 's-resize', height: '4px', left: '0px', right: '0px', borderBottom: aBorder },
				'ui-resizable-e': { cursor: 'e-resize', width: '4px', top: '0px', bottom: '0px', borderRight: aBorder },
				'ui-resizable-w': { cursor: 'w-resize', width: '4px', top: '0px', bottom: '0px', borderLeft: aBorder },
				'ui-resizable-se': { cursor: 'se-resize', width: '4px', height: '4px', borderRight: aBorder, borderBottom: aBorder },
				'ui-resizable-sw': { cursor: 'sw-resize', width: '4px', height: '4px', borderBottom: aBorder, borderLeft: aBorder },
				'ui-resizable-ne': { cursor: 'ne-resize', width: '4px', height: '4px', borderRight: aBorder, borderTop: aBorder },
				'ui-resizable-nw': { cursor: 'nw-resize', width: '4px', height: '4px', borderLeft: aBorder, borderTop: aBorder }
			};
			
			o.knobTheme = {
				'ui-resizable-handle': { background: '#F2F2F2', border: '1px solid #808080', height: '8px', width: '8px' },
				'ui-resizable-n': { cursor: 'n-resize', top: '-4px', left: '45%' },
				'ui-resizable-s': { cursor: 's-resize', bottom: '-4px', left: '45%' },
				'ui-resizable-e': { cursor: 'e-resize', right: '-4px', top: '45%' },
				'ui-resizable-w': { cursor: 'w-resize', left: '-4px', top: '45%' },
				'ui-resizable-se': { cursor: 'se-resize', right: '-4px', bottom: '-4px' },
				'ui-resizable-sw': { cursor: 'sw-resize', left: '-4px', bottom: '-4px' },
				'ui-resizable-nw': { cursor: 'nw-resize', left: '-4px', top: '-4px' },
				'ui-resizable-ne': { cursor: 'ne-resize', right: '-4px', top: '-4px' }
			};
			
			o._nodeName = this.element[0].nodeName;
		
			//Wrap the element if it cannot hold child nodes
			if(o._nodeName.match(/textarea|input|select|button|img/i)) {
				var el = this.element;
				
				//Opera fixing relative position
				if (/relative/.test(el.css('position')) && $.browser.opera)
					el.css({ position: 'relative', top: 'auto', left: 'auto' });
				
				//Create a wrapper element and set the wrapper to the new current internal element
				el.wrap(
					$('<div class="ui-wrapper"	style="overflow: hidden;"></div>').css( {
						position: el.css('position'),
						width: el.outerWidth(),
						height: el.outerHeight(),
						top: el.css('top'),
						left: el.css('left')
					})
				);
				
				var oel = this.element; this.element = this.element.parent();
		
				//Move margins to the wrapper
				this.element.css({ marginLeft: oel.css("marginLeft"), marginTop: oel.css("marginTop"),
					marginRight: oel.css("marginRight"), marginBottom: oel.css("marginBottom")
				});
		
				oel.css({ marginLeft: 0, marginTop: 0, marginRight: 0, marginBottom: 0});
		
				//Prevent Safari textarea resize
				if ($.browser.safari && o.preventDefault) oel.css('resize', 'none');
		
				o.proportionallyResize = oel.css({ position: 'static', zoom: 1, display: 'block' });
				
				// avoid IE jump
				this.element.css({ margin: oel.css('margin') });
				
				// fix handlers offset
				this._proportionallyResize();
			}
			
			if(!o.handles) o.handles = !$('.ui-resizable-handle', this.element).length ? "e,s,se" : { n: '.ui-resizable-n', e: '.ui-resizable-e', s: '.ui-resizable-s', w: '.ui-resizable-w', se: '.ui-resizable-se', sw: '.ui-resizable-sw', ne: '.ui-resizable-ne', nw: '.ui-resizable-nw' };
			if(o.handles.constructor == String) {
		
				if(o.handles == 'all') o.handles = 'n,e,s,w,se,sw,ne,nw';
		
				var n = o.handles.split(","); o.handles = {};
		
				o.zIndex = o.zIndex || 1000;
				
				// insertions are applied when don't have theme loaded
				var insertionsDefault = {
					handle: 'position: absolute; display: none; overflow:hidden;',
					n: 'top: 0pt; width:100%;',
					e: 'right: 0pt; height:100%;',
					s: 'bottom: 0pt; width:100%;',
					w: 'left: 0pt; height:100%;',
					se: 'bottom: 0pt; right: 0px;',
					sw: 'bottom: 0pt; left: 0px;',
					ne: 'top: 0pt; right: 0px;',
					nw: 'top: 0pt; left: 0px;'
				};
		
				for(var i = 0; i < n.length; i++) {
					var handle = jQuery.trim(n[i]), dt = o.defaultTheme, hname = 'ui-resizable-'+handle, loadDefault = !$.ui.css(hname) && !o.knobHandles, userKnobClass = $.ui.css('ui-resizable-knob-handle'), 
								allDefTheme = $.extend(dt[hname], dt['ui-resizable-handle']), allKnobTheme = $.extend(o.knobTheme[hname], !userKnobClass ? o.knobTheme['ui-resizable-handle'] : {});
					
					// increase zIndex of sw, se, ne, nw axis
					var applyZIndex = /sw|se|ne|nw/.test(handle) ? { zIndex: ++o.zIndex } : {};
					
					var defCss = (loadDefault ? insertionsDefault[handle] : ''), 
						axis = $(['<div class="ui-resizable-handle ', hname, '" style="', defCss, insertionsDefault.handle, '"></div>'].join('')).css( applyZIndex );
					o.handles[handle] = '.ui-resizable-'+handle;
					
					this.element.append(
						//Theme detection, if not loaded, load o.defaultTheme
						axis.css( loadDefault ? allDefTheme : {} )
							// Load the knobHandle css, fix width, height, top, left...
							.css( o.knobHandles ? allKnobTheme : {} ).addClass(o.knobHandles ? 'ui-resizable-knob-handle' : '').addClass(o.knobHandles)
					);
				}
				
				if (o.knobHandles) this.element.addClass('ui-resizable-knob').css( !$.ui.css('ui-resizable-knob') ? { /*border: '1px #fff dashed'*/ } : {} );
			}

			this._renderAxis = function(target) {
				target = target || this.element;
		
				for(var i in o.handles) {
					if(o.handles[i].constructor == String) 
						o.handles[i] = $(o.handles[i], this.element).show();
		
					if (o.transparent)
						o.handles[i].css({opacity:0});
		
					//Apply pad to wrapper element, needed to fix axis position (textarea, inputs, scrolls)
					if (this.element.is('.ui-wrapper') && 
						o._nodeName.match(/textarea|input|select|button/i)) {
		
						var axis = $(o.handles[i], this.element), padWrapper = 0;
		
						//Checking the correct pad and border
						padWrapper = /sw|ne|nw|se|n|s/.test(i) ? axis.outerHeight() : axis.outerWidth();
		
						//The padding type i have to apply...
						var padPos = [ 'padding', 
							/ne|nw|n/.test(i) ? 'Top' :
							/se|sw|s/.test(i) ? 'Bottom' : 
							/^e$/.test(i) ? 'Right' : 'Left' ].join(""); 
		
						if (!o.transparent)
							target.css(padPos, padWrapper);
		
						this._proportionallyResize();
					}
					if(!$(o.handles[i]).length) continue;
				}
			};
			
			this._renderAxis(this.element);
			o._handles = $('.ui-resizable-handle', self.element);
			
			if (o.disableSelection)
				o._handles.each(function(i, e) { $.ui.disableSelection(e); });
			
			//Matching axis name
			o._handles.mouseover(function() {
				if (!o.resizing) {
					if (this.className) 
						var axis = this.className.match(/ui-resizable-(se|sw|ne|nw|n|e|s|w)/i);
					//Axis, default = se
					self.axis = o.axis = axis && axis[1] ? axis[1] : 'se';
				}
			});
					
			//If we want to auto hide the elements
			if (o.autohide) {
				o._handles.hide();
				$(self.element).addClass("ui-resizable-autohide").hover(function() {
					$(this).removeClass("ui-resizable-autohide");
					o._handles.show();
				},
				function(){
					if (!o.resizing) {
						$(this).addClass("ui-resizable-autohide");
						o._handles.hide();
					}
				});
			}
		
			//Initialize mouse events for interaction
			this.element.mouse({
				executor: this,
				delay: 0,
				distance: 0,
				dragPrevention: ['input','textarea','button','select','option'],
				start: this.start,
				stop: this.stop,
				drag: this.drag,
				condition: function(e) {
					if(this.disabled) return false;
					for(var i in this.options.handles) {
						if($(this.options.handles[i])[0] == e.target) return true;
					}
					return false;
				}
			});
			
		},
		plugins: {},
		ui: function() {
			return {
				instance: this,
				axis: this.options.axis,
				options: this.options
			};
		},
		_renderProxy: function() {
			var el = this.element, o = this.options;
			this.elementOffset = el.offset();
	
			if(o.proxy) {
				this.helper = this.helper || $('<div style="overflow:hidden;"></div>');
	
				// fix ie6 offset
				var ie6 = $.browser.msie && $.browser.version < 7, ie6offset = (ie6 ? 1 : 0),
				pxyoffset = ( ie6 ? 2 : -1 );
	
				this.helper.addClass(o.proxy).css({
					width: el.outerWidth() + pxyoffset,
					height: el.outerHeight() + pxyoffset,
					position: 'absolute',
					left: this.elementOffset.left - ie6offset +'px',
					top: this.elementOffset.top - ie6offset +'px',
					zIndex: ++o.zIndex
				});
				
				this.helper.appendTo("body");
	
				if (o.disableSelection)
					$.ui.disableSelection(this.helper.get(0));
	
			} else {
				this.helper = el; 
			}
		},
		propagate: function(n,e) {
			$.ui.plugin.call(this, n, [e, this.ui()]);
			this.element.triggerHandler(n == "resize" ? n : ["resize", n].join(""), [e, this.ui()], this.options[n]);
		},
		destroy: function() {
			var el = this.element, wrapped = el.children(".ui-resizable").get(0),
			
			_destroy = function(exp) {
				$(exp).removeClass("ui-resizable ui-resizable-disabled")
					.mouse("destroy").removeData("resizable").unbind(".resizable").find('.ui-resizable-handle').remove();
			};
			
			_destroy(el);
			
			if (el.is('.ui-wrapper') && wrapped) {
				el.parent().append(
					$(wrapped).css({
						position: el.css('position'),
						width: el.outerWidth(),
						height: el.outerHeight(),
						top: el.css('top'),
						left: el.css('left')
					})
				).end().remove();
				
				_destroy(wrapped);
			}
		},
		enable: function() {
			this.element.removeClass("ui-resizable-disabled");
			this.disabled = false;
		},
		disable: function() {
			this.element.addClass("ui-resizable-disabled");
			this.disabled = true;
		},
		start: function(e) {
			var o = this.options, iniPos = this.element.position(), el = this.element, 
				num = function(v) { return parseInt(v, 10) || 0; }, ie6 = $.browser.msie && $.browser.version < 7;
			o.resizing = true;
			o.documentScroll = { top: $(document).scrollTop(), left: $(document).scrollLeft() };
	
			// bugfix #1749
			if (el.is('.ui-draggable') || (/absolute/).test(el.css('position'))) {
				
				// sOffset decides if document scrollOffset will be added to the top/left of the resizable element
				var sOffset = $.browser.msie && !o.containment && (/absolute/).test(el.css('position')) && !(/relative/).test(el.parent().css('position'));
				var dscrollt = sOffset ? o.documentScroll.top : 0, dscrolll = sOffset ? o.documentScroll.left : 0;
				
				el.css({ position: 'absolute', top: (iniPos.top + dscrollt), left: (iniPos.left + dscrolll) });
			}
			
			//Opera fixing relative position
			if (/relative/.test(el.css('position')) && $.browser.opera)
				el.css({ position: 'relative', top: 'auto', left: 'auto' });
	
			this._renderProxy();
	
			var curleft = num(this.helper.css('left')), curtop = num(this.helper.css('top'));
			
			//Store needed variables
			this.offset = this.helper.offset();
			this.position = { left: curleft, top: curtop };
			this.size = o.proxy || ie6 ? { width: el.outerWidth(), height: el.outerHeight() } : { width: el.width(), height: el.height() };
			this.originalSize = o.proxy || ie6 ? { width: el.outerWidth(), height: el.outerHeight() } : { width: el.width(), height: el.height() };
			this.originalPosition = { left: curleft, top: curtop };
			this.sizeDiff = { width: el.outerWidth() - el.width(), height: el.outerHeight() - el.height() };
			this.originalMousePosition = { left: e.pageX, top: e.pageY };
			
			//Aspect Ratio
			o.aspectRatio = (typeof o.aspectRatio == 'number') ? o.aspectRatio : ((this.originalSize.height / this.originalSize.width)||1);
	
			if (o.preserveCursor)
				$('body').css('cursor', this.axis + '-resize');
				
			this.propagate("start", e);
			return false;
		},
		stop: function(e) {
			this.options.resizing = false;
			var o = this.options, num = function(v) { return parseInt(v, 10) || 0; }, self = this;
	
			if(o.proxy) {
				var pr = o.proportionallyResize, ista = pr && (/textarea/i).test(pr.get(0).nodeName), 
							soffseth = ista && $.ui.hasScroll(pr.get(0), 'left') /* TODO - jump height */ ? 0 : self.sizeDiff.height,
								soffsetw = ista ? 0 : self.sizeDiff.width;
			
				var style = { width: (self.size.width - soffsetw), height: (self.size.height - soffseth) },
						left = parseInt(self.element.css('left'), 10) + (self.position.left - self.originalPosition.left), 
							top = parseInt(self.element.css('top'), 10) + (self.position.top - self.originalPosition.top);
				
				if (!o.animate)
					this.element.css($.extend(style, { top: top, left: left }));
				
				if (o.proxy && !o.animate) this._proportionallyResize();
				this.helper.remove();
			}

			if (o.preserveCursor)
			$('body').css('cursor', 'auto');
	
			this.propagate("stop", e);	
			return false;
		},
		drag: function(e) {
			//Increase performance, avoid regex
			var el = this.helper, o = this.options, props = {},
				self = this, smp = this.originalMousePosition, a = this.axis;

			var dx = (e.pageX-smp.left)||0, dy = (e.pageY-smp.top)||0;
			var trigger = this.change[a];
			if (!trigger) return false;
			
			// Calculate the attrs that will be change
			var data = trigger.apply(this, [e, dx, dy]), ie6 = $.browser.msie && $.browser.version < 7, csdif = this.sizeDiff;
			
			if (o._aspectRatio || e.shiftKey)
				data = this._updateRatio(data, e);
			
			data = this._respectSize(data, e);
			
			this.propagate("resize", e);
			
			el.css({
				top: this.position.top + "px", left: this.position.left + "px", 
				width: this.size.width + "px", height: this.size.height + "px"
			});
			
			if (!o.proxy && o.proportionallyResize)
				this._proportionallyResize();
			
			this._updateCache(data);
			
			return false;
		},
		
		_updateCache: function(data) {
			var o = this.options;
			this.offset = this.helper.offset();
			if (data.left) this.position.left = data.left;
			if (data.top) this.position.top = data.top;
			if (data.height) this.size.height = data.height;
			if (data.width) this.size.width = data.width;
		},
		
		_updateRatio: function(data, e) {
			var o = this.options, cpos = this.position, csize = this.size, a = this.axis;
			
			if (data.height) data.width = Math.round(csize.height / o.aspectRatio);
			else if (data.width) data.height = Math.round(csize.width * o.aspectRatio);
			
			if (a == 'sw') {
				data.left = cpos.left + (csize.width - data.width);
				data.top = null;
			}
			if (a == 'nw') { 
				data.top = cpos.top + (csize.height - data.height);
				data.left = cpos.left + (csize.width - data.width);
			}
			
			return data;
		},
		
		_respectSize: function(data, e) {
			
			var el = this.helper, o = this.options, pRatio = o._aspectRatio || e.shiftKey, a = this.axis, 
					ismaxw = data.width && o.maxWidth && o.maxWidth < data.width, ismaxh = data.height && o.maxHeight && o.maxHeight < data.height,
						isminw = data.width && o.minWidth && o.minWidth > data.width, isminh = data.height && o.minHeight && o.minHeight > data.height;
			
			if (isminw) data.width = o.minWidth;
			if (isminh) data.height = o.minHeight;
			if (ismaxw) data.width = o.maxWidth;
			if (ismaxh) data.height = o.maxHeight;
			
			var dw = this.originalPosition.left + this.originalSize.width, dh = this.position.top + this.size.height;
			var cw = /sw|nw|w/.test(a), ch = /nw|ne|n/.test(a);
			
			if (isminw && cw) data.left = dw - o.minWidth;
			if (ismaxw && cw) data.left = dw - o.maxWidth;
			if (isminh && ch)	data.top = dh - o.minHeight;
			if (ismaxh && ch)	data.top = dh - o.maxHeight;
			
			// fixing jump error on top/left - bug #2330
			var isNotwh = !data.width && !data.height;
			if (isNotwh && !data.left && data.top) data.top = null;
			else if (isNotwh && !data.top && data.left) data.left = null;
			
			return data;
		},
		
		_proportionallyResize: function() {
			var o = this.options;
			if (!o.proportionallyResize) return;
			var prel = o.proportionallyResize, el = this.helper || this.element;
			
			if (!o.borderDif) {
				var b = [prel.css('borderTopWidth'), prel.css('borderRightWidth'), prel.css('borderBottomWidth'), prel.css('borderLeftWidth')],
					p = [prel.css('paddingTop'), prel.css('paddingRight'), prel.css('paddingBottom'), prel.css('paddingLeft')];
				
				o.borderDif = $.map(b, function(v, i) {
					var border = parseInt(v,10)||0, padding = parseInt(p[i],10)||0;
					return border + padding; 
				});
			}
			prel.css({
				height: (el.height() - o.borderDif[0] - o.borderDif[2]) + "px",
				width: (el.width() - o.borderDif[1] - o.borderDif[3]) + "px"
			});
		},
		
		change: {
			e: function(e, dx, dy) {
				return { width: this.originalSize.width + dx };
			},
			w: function(e, dx, dy) {
				var o = this.options, cs = this.originalSize, sp = this.originalPosition;
				return { left: sp.left + dx, width: cs.width - dx };
			},
			n: function(e, dx, dy) {
				var o = this.options, cs = this.originalSize, sp = this.originalPosition;
				return { top: sp.top + dy, height: cs.height - dy };
			},
			s: function(e, dx, dy) {
				return { height: this.originalSize.height + dy };
			},
			se: function(e, dx, dy) {
				return $.extend(this.change.s.apply(this, arguments), this.change.e.apply(this, [e, dx, dy]));
			},
			sw: function(e, dx, dy) {
				return $.extend(this.change.s.apply(this, arguments), this.change.w.apply(this, [e, dx, dy]));
			},
			ne: function(e, dx, dy) {
				return $.extend(this.change.n.apply(this, arguments), this.change.e.apply(this, [e, dx, dy]));
			},
			nw: function(e, dx, dy) {
				return $.extend(this.change.n.apply(this, arguments), this.change.w.apply(this, [e, dx, dy]));
			}
		}
	});
	
	$.extend($.ui.resizable, {
		defaults: {
			preventDefault: true,
			transparent: false,
			minWidth: 10,
			minHeight: 10,
			aspectRatio: false,
			disableSelection: true,
			preserveCursor: true,
			autohide: false,
			knobHandles: false
		}
	});

/*
 * Resizable Extensions
 */

	$.ui.plugin.add("resizable", "containment", {
		
		start: function(e, ui) {
			var o = ui.options, self = ui.instance, el = self.element;
			var oc = o.containment,	ce = (oc instanceof jQuery) ? oc.get(0) : (/parent/.test(oc)) ? el.parent().get(0) : oc;
			if (!ce) return;
			
			if (/document/.test(oc) || oc == document) {
				self.containerOffset = { left: 0, top: 0 };

				self.parentData = { 
					element: $(document), left: 0, top: 0, width: $(document).width(),
					height: $(document).height() || document.body.parentNode.scrollHeight
				};
			}
			
			// i'm a node, so compute top, left, right, bottom
			else{
				self.containerOffset = $(ce).offset();
				self.containerSize = { height: $(ce).innerHeight(), width: $(ce).innerWidth() };
			
				var co = self.containerOffset, ch = self.containerSize.height,	cw = self.containerSize.width, 
							width = ($.ui.hasScroll(ce, "left") ? ce.scrollWidth : cw ), height = ($.ui.hasScroll(ce) ? ce.scrollHeight : ch);
			
				self.parentData = { 
					element: ce, left: co.left, top: co.top, width: width, height: height
				};
			}
		},
		
		resize: function(e, ui) {
			var o = ui.options, self = ui.instance, ps = self.containerSize, 
						co = self.containerOffset, cs = self.size, cp = self.position,
							pRatio = o._aspectRatio || e.shiftKey;
			
			if (cp.left < (o.proxy ? co.left : 0)) {
				self.size.width = self.size.width + (o.proxy ? (self.position.left - co.left) : self.position.left);
				if (pRatio) self.size.height = self.size.width * o.aspectRatio;
				self.position.left = o.proxy ? co.left : 0;
			}
			
			if (cp.top < (o.proxy ? co.top : 0)) {
				self.size.height = self.size.height + (o.proxy ? (self.position.top - co.top) : self.position.top);
				if (pRatio) self.size.width = self.size.height / o.aspectRatio;
				self.position.top = o.proxy ? co.top : 0;
			}
			
			var woset = (o.proxy ? self.offset.left - co.left : self.position.left) + self.sizeDiff.width, 
						hoset = (o.proxy ? self.offset.top - co.top : self.position.top) + self.sizeDiff.height;
			
			if (woset + self.size.width >= self.parentData.width) {
				self.size.width = self.parentData.width - woset;
				if (pRatio) self.size.height = self.size.width * o.aspectRatio;
			}
			
			if (hoset + self.size.height >= self.parentData.height) {
				self.size.height = self.parentData.height - hoset;
				if (pRatio) self.size.width = self.size.height / o.aspectRatio;
			}
		}
	});
	
	$.ui.plugin.add("resizable", "grid", {
		
		resize: function(e, ui) {
			var o = ui.options, self = ui.instance, cs = self.size, os = self.originalSize, op = self.originalPosition, a = self.axis, ratio = o._aspectRatio || e.shiftKey;
			o.grid = typeof o.grid == "number" ? [o.grid, o.grid] : o.grid;
			var ox = Math.round((cs.width - os.width) / o.grid[0]) * o.grid[0], oy = Math.round((cs.height - os.height) / o.grid[1]) * o.grid[1];
			
			if (/^(se|s|e)$/.test(a)) {
				self.size.width = os.width + ox;
				self.size.height = os.height + oy;
			}
			else if (/^(ne)$/.test(a)) {
				self.size.width = os.width + ox;
				self.size.height = os.height + oy;
				self.position.top = op.top - oy;
			}
			else if (/^(sw)$/.test(a)) {
				self.size.width = os.width + ox;
				self.size.height = os.height + oy;
				self.position.left = op.left - ox;
			}
			else {
				self.size.width = os.width + ox;
				self.size.height = os.height + oy;
				self.position.top = op.top - oy;
				self.position.left = op.left - ox;
			}
		}
		
	});
	
	$.ui.plugin.add("resizable", "animate", {
		
		stop: function(e, ui) {
			var o = ui.options, self = ui.instance;

			var pr = o.proportionallyResize, ista = pr && (/textarea/i).test(pr.get(0).nodeName), 
							soffseth = ista && $.ui.hasScroll(pr.get(0), 'left') /* TODO - jump height */ ? 0 : self.sizeDiff.height,
								soffsetw = ista ? 0 : self.sizeDiff.width;
			
			var style = { width: (self.size.width - soffsetw), height: (self.size.height - soffseth) },
						left = parseInt(self.element.css('left'), 10) + (self.position.left - self.originalPosition.left), 
							top = parseInt(self.element.css('top'), 10) + (self.position.top - self.originalPosition.top); 
			
			self.element.animate(
				$.extend(style, { top: top, left: left }),
				{ 
					duration: o.animateDuration || "slow", 
					easing: o.animateEasing || "swing", 
					step: function() {
						if (pr) pr.css({ width: self.element.css('width'), height: self.element.css('height') });
					}
				}
			);
		}
		
	});
	
	$.ui.plugin.add("resizable", "ghost", {
		
		start: function(e, ui) {
			var o = ui.options, self = ui.instance, pr = o.proportionallyResize, cs = self.size;
			
			if (!pr) self.ghost = self.element.clone();
			else self.ghost = pr.clone();
			
			self.ghost.css(
				{ opacity: .25, display: 'block', position: 'relative', height: cs.height, width: cs.width, margin: 0, left: 0, top: 0 }
			)
			.addClass('ui-resizable-ghost').addClass(typeof o.ghost == 'string' ? o.ghost : '');
			
			self.ghost.appendTo(self.helper);
			
		},
		
		resize: function(e, ui){
			var o = ui.options, self = ui.instance, pr = o.proportionallyResize;
			
			if (self.ghost) self.ghost.css({ position: 'relative', height: self.size.height, width: self.size.width });
			
		},
		
		stop: function(e, ui){
			var o = ui.options, self = ui.instance, pr = o.proportionallyResize;
			if (self.ghost && self.helper) self.helper.get(0).removeChild(self.ghost.get(0));
		}
		
	});

})(jQuery);
