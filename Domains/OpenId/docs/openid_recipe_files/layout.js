var timeOutHandle;
function openMoreMenu() {
  var menu = document.getElementById('dropDownMenu');
  if (menu && menu.style) menu.style.display = '';
  if (typeof(timeOutHandle) !== undefined || timeOutHandle !== null ) {
    clearTimeout(timeOutHandle);
  }

  var moreItem = document.getElementById("more_tab");
  if(moreItem) {
    if(moreItem.offsetLeft) wide = moreItem.offsetLeft; else wide = 351;
    document.getElementById("dropDownMenu").style.left = (wide - 1) + "px";
  }

  if (typeof dojo == 'undefined') return;
  var moreTab = dojo.query('.moreTab');
  moreTab[0].className = 'moreTab tabsListItem selectedMore';  
  if (dojo.isIE && dojo.isIE < 7 && typeof enableIFrame != 'undefined') enableIFrame(menu, menu.clientWidth, menu.clientHeight + 44); // Add the top margin to the menu height
}

function hideMoreMenu() {
  timeOutHandle = setTimeout(function() { hideMoreMenuNow(); }, 250);    
}

function hideMoreMenuNow() {
  if (typeof(timeOutHandle) !== undefined || timeOutHandle !== null ) {
    clearTimeout(timeOutHandle);
  }
  var menu = document.getElementById('dropDownMenu');
  if (menu && menu.style) menu.style.display = 'none';
  if (typeof dojo == 'undefined') return;
  if (menu) {
      if (dojo.isIE && dojo.isIE < 7) {
        var ifId = menu.id + '_iframe';
        var iFrame = document.getElementById(ifId);
        if(iFrame && iFrame.style) hide(iFrame);
      }
  }
  var moreTab = dojo.query('.moreTab');
  if (moreTab[0]) moreTab[0].className = 'moreTab tabsListItem';
}
