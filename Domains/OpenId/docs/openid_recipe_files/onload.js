// PLX_NO_PICKLE
/*
 * Onload script original from:
 * <http://javascript.about.com/library/scripts/blsafeonload.htm>
 *
 * Also see additional enhancements imported from:
 * <http://dean.edwards.name/weblog/2005/09/busted/>
 * - terry chay
 * TODO: move browser detection elsewhere...
 */
// Browser Detection
isMac = (navigator.appVersion.indexOf("Mac")!=-1) ? true : false;
NS4 = (document.layers) ? true : false;
IEmac = ((document.all)&&(isMac)) ? true : false;
IE4plus = (document.all) ? true : false;
IE4 = ((document.all)&&(navigator.appVersion.indexOf("MSIE 4.")!=-1)) ? true : false;
IE5 = ((document.all)&&(navigator.appVersion.indexOf("MSIE 5.")!=-1)) ? true : false;
ver4 = (NS4 || IE4plus) ? true : false;
NS6 = (!document.layers) && (navigator.userAgent.indexOf('Netscape')!=-1)?true:false;

// Body onload utility (supports multiple onload functions)
var gSafeOnload = new Array();
var gSafeOnloadInit = false;
function SafeOnloadInit() {
   if (gSafeOnloadInit) return;

   gSafeOnloadInit = true;
   /* for Mozilla */
   if (document.addEventListener) {
       document.addEventListener("DOMContentLoaded", SafeOnload, null);
   }

   /* for Internet Explorer */
   /*@cc_on @*/
   /*@if (@_win32)
       document.write('<script defer src="/css/m/js/ie_onload.js"><'+'/script>');
   /*@end @*/

   /* for other browsers */
   window.onload = SafeOnload;


}
function SafeAddOnload(f)
{
	if (IEmac && IE4)  // IE 4.5 blows out on testing window.onload
	{
		SafeOnloadInit();
		gSafeOnload[gSafeOnload.length] = f;
	}
	else if (window.onload)
	{
		if (window.onload != SafeOnload)
		{
			gSafeOnload[0] = window.onload;
			SafeOnloadInit();
		}
		gSafeOnload[gSafeOnload.length] = f;
	}
	else
	{
		SafeOnloadInit();
    gSafeOnload[gSafeOnload.length] = f;
	}
}

function SafeOnload()
{
    // quit if this function has already been called
    if (arguments.callee.done) return;

    // flag this function so we don't do the same thing twice
    arguments.callee.done = true;

	for (var i=0;i<gSafeOnload.length;i++) {
		gSafeOnload[i]();
	}
}

// Call the following with your function as the argument
//SafeAddOnload(yourfunctioname);
