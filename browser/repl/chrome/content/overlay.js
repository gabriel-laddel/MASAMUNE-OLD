window.addEventListener('load', function (event) {
    return mozrepl.initOverlay();
}, false);
var mozrepl = {  };
Components.classes['@mozilla.org/moz/jssubscript-loader;1'].getService(Components.interfaces.mozIJSSubScriptLoader).loadSubScript('chrome://mozrepl/content/overlay_impl.js', mozrepl);
window.addEventListener('load', function (event) {
    return mozrepl.initOverlay();
}, false);
var mozrepl = {  };
Components.classes['@mozilla.org/moz/jssubscript-loader;1'].getService(Components.interfaces.mozIJSSubScriptLoader).loadSubScript('chrome://mozrepl/content/overlay_impl.js', mozrepl);
