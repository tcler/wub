(function($){$.fn.HoverImageText=function(){var opts=$.fn.HoverImageText.defaults;return this.each(function(){var oText=$(this).children(opts.TagName);oText=oText.hide();var oImg=$(this).hover(function(){oHover=oText;window.setTimeout(function(){HoverCheckAndShow(oText)},opts.HoverCheck)},function(){oHover=null;oText.animate(opts.AnimHide,opts.HoverOut,opts.AnimHideCallback)})})};$.fn.HoverImageText.defaults={AnimShow:{opacity:"show"},AnimShowCallback:null,HoverCheck:600,HoverIn:500,AnimHide:{opacity:"hide"},AnimHideCallback:null,HoverOut:300,TagName:'p'};function HoverCheckAndShow(oText){var opts=$.fn.HoverImageText.defaults;if(oHover==oText){oText.animate(opts.AnimShow,opts.HoverIn,function(){this.style.display="inline";if(typeof(opts.AnimShowCallback)=='function'){opts.AnimShowCallback()}});oText.each(function(){this.style.display="inline"});if($.browser.msie){oText.each(function(){var oThis=this;window.setTimeout(function(){oThis.style.filter="progid:DXImageTransform.Microsoft.Alpha(opacity=80)"},opts.HoverIn-260)})}}return oText};var oHover=null;$.fx.prototype.update=function(){if(this.options.step)this.options.step.apply(this.elem,[this.now,this]);(jQuery.fx.step[this.prop]||jQuery.fx.step._default)(this);if(this.prop=="height"||this.prop=="width")this.elem.style.display="inline"}})(jQuery);
