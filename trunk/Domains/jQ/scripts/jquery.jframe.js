// jFrame
// $Revision: 1.80 $
// Author: Frederic de Zorzi
// Contact: fredz@_nospam_pimentech.net
// Revision: $Revision: 1.80 $
// Date: $Date: 2007/11/08 15:17:18 $
// Copyright: 2007 PimenTech SARL
// Tags: ajax javascript pimentech english jquery



jQuery.fn.waitingJFrame = function () {
	// Overload this function in your code to place a waiting event 
	// message, like : 	$(this).html("<b>loading...</b>");
}


jQuery.fn.getJFrameTarget = function () {
    // Returns first parent jframe element, if exists
    var div = jQuery(this).parents("div[@src]").get(0);
    if (div) {
        var target = jQuery(this).attr("target");
        if (target) {
            return jQuery("#" + target);
        }
    }
    return jQuery(div);
};


jQuery.fn.loadJFrame = function(url, callback) {
    // like ajax.load, for jFrame. the onload attribute is supported
    var this_callback = jQuery(this).attr("onload");
    callback = callback || function(){};
    url = url || jQuery(this).attr("src");
    if (url && url != "#") {
		jQuery(this).waitingJFrame();
        jQuery(this).load(url, 
                     function() { 
                         jQuery(this).attr("src", url);
                         jQuery(this).activateJFrame(); 
                         jQuery(this).find("div[@src]").each(function(i) {
                                 jQuery(this).loadJFrame();
                             } );
                         eval(this_callback);
                         callback();
                     });
    }
    else {
        jQuery(this).activateJFrame(); 
    }
};

jQuery.fn.activateJFrame = function() {
    // Add an onclick event on all <a> and <input type="submit"> tags
    jQuery(this).find("a")
	.not("[jframe='no']")
    .unbind("click")
    .click(function() { 
            var target = jQuery(this).getJFrameTarget();
            if (target.length) {
                var href = jQuery(this).attr("href");
                if (href && href.indexOf('javascript:') != 0) {
                    target.loadJFrame(href);
                    return false;
                }
            }
            return true;
        } );

    jQuery(this).find("input[@type='submit']", "button[@type='submit']")
	.not("[jframe='no']")
    .unbind("click")
    .click(function() {
            var input = this;
            var target = jQuery(input).getJFrameTarget();
            if (target.length) {
                var form = input.form;
                if (form.onsubmit && form.onsubmit()==false) {
                    return false;
                }
				target.waitingJFrame();
                jQuery(form).ajaxSubmit({ 
                        target: target,
                        beforeSubmit: function(formArray) { 
                            formArray.push({ 
                                name:"submit", 
                                value: jQuery(input).attr("value") 
                            }); 
                        },
                        success: function() { 
                            target.attr("src", jQuery(form).attr("action"));
                            eval(target.attr("onload"));
                            target.activateJFrame(); 
                        }
                    });
                return false;
            }
            return true;
        } );

	jQuery(this).find("form")
    .unbind("submit")
    .submit(function() {
			this.find("input[@type='submit']", "button[@type='submit']")[0].click();
		} ); 
};


jQuery(document).ready(function() { 
    jQuery(document).find("div[@src]").each(function(i) {
            jQuery(this).loadJFrame();
            if (jQuery(this).attr("src") && jQuery(this).attr("src") != "#") {
 }

        } );
} );
