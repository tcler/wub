jQuery.Accordion.defaults.animated = false;

function state(accordion) {
	var args = $.makeArray(arguments).slice(1);
	$.each(args, function(i, n) {
		equals(n, accordion.find("p").eq(i).is(":visible"));
	});
}

test("basics", function() {
	state($('#list1').Accordion(), 1, 0, 0, 0, 0);
});

test("autoheight", function() {
	$('#navigation').Accordion({ header: '.head',autoheight: false });
	equals( 90, $('#navigation ul:first').height() );
	equals( 126, $('#navigation ul:eq(1)').height() );
	equals( 54, $('#navigation ul:last').height() );
	$('#navigation').unbind().find("*").unbind().end().Accordion({ header: '.head',autoheight: true });
	equals( 126, $('#navigation ul:first').height() );
	equals( 126, $('#navigation ul:eq(1)').height() );
	equals( 126, $('#navigation ul:last').height() );
});

test("activate, numeric", function() {
	var ac = $('#list1').Accordion({ active: 1 });
	state(ac, 0, 1, 1, 0, 0);
	ac.activate(2);
	state(ac, 0, 0, 0, 1, 1);
	ac.activate(0);
	state(ac, 1, 0, 0, 0, 0);
	ac.activate(1);
	state(ac, 0, 1, 1, 0, 0);
	ac.activate(2);
	state(ac, 0, 0, 0, 1, 1);
	ac.activate(-1);
	state(ac, 0, 0, 0, 1, 1);
});

test("activate, boolean, alwaysOpen:false", function() {
	var ac = $('#list1').Accordion({alwaysOpen: false}).activate(2);
	state(ac, 0, 0, 0, 1, 1);
	ac.activate(false);
	state(ac, 0, 0, 0, 0, 0);
	ac.activate(0);
	state(ac, 1, 0, 0, 0, 0);
	ac.activate(-1);
	state(ac, 0, 0, 0, 0, 0);
});

test("activate, boolean, alwaysOpen:true", function() {
	var ac = $('#list1').Accordion().activate(2);
	state(ac, 0, 0, 0, 1, 1);
	ac.activate(false);
	state(ac, 0, 0, 0, 1, 1);
});

test("activate, string expression", function() {
	var ac = $('#list1').Accordion({ active: ":last" });
	state(ac, 0, 0, 0, 1, 1);
	ac.activate(":first");
	state(ac, 1, 0, 0, 0, 0);
	ac.activate(":eq(1)");
	state(ac, 0, 1, 1, 0, 0);
	ac.activate(":last");
	state(ac, 0, 0, 0, 1, 1);
});

test("activate, jQuery or DOM element", function() {
	var ac = $('#list1').Accordion({ active: $("#list1 h3:last") });
	state(ac, 0, 0, 0, 1, 1);
	ac.activate($("#list1 h3:first"));
	state(ac, 1, 0, 0, 0, 0);
	ac.activate($("#list1 h3")[1]);
	state(ac, 0, 1, 1, 0, 0);
});