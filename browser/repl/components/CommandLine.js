var Cc = Components.classes;
var Ci = Components.interfaces;
var Cr = Components.results;
var Cu = Components.utils;

var CATEGORY = 'c-mozrepl';
var CLASS_ID = Components.ID('{f62cbe68-ee70-4264-8586-66df185244f5}');
var CONTRACT_ID = '@mozilla.org/commandlinehandler/general-startup;1?type=repl';
var INTERFACE = Ci.nsICommandLineHandler;

Cu.import("resource://gre/modules/XPCOMUtils.jsm");

var srvPref = Components.classes['@mozilla.org/preferences-service;1']
    .getService(Components.interfaces.nsIPrefService)
    .getBranch('extensions.mozrepl.');


function MozReplCommandLineHandler() {}

MozReplCommandLineHandler.prototype = {
    classDescription: "MozRepl command line handler",
    classID: CLASS_ID,
    contactID: CONTRACT_ID,
    QueryInterface: XPCOMUtils.generateQI([Ci.nsICommandLineHandler]),

    handle: function(cmdLine) {
        var start;
        try {
            start = cmdLine.handleFlag('repl', false);
        } catch (e) {}

        var contextWindowType;
        try {
            contextWindowType = cmdLine.handleFlagWithParam('repl-context', false);
        } catch(e) {}

        if(start || contextWindowType) {
            var port = Number(cmdLine.handleFlagWithParam('repl', false)) ||
                srvPref.getIntPref('port');
            var loopbackOnly = srvPref.getBoolPref('loopbackOnly');

            var service = Cc['@hyperstruct.net/mozlab/mozrepl;1']
                .getService(Ci.nsIMozRepl)
                .wrappedJSObject;

            if(contextWindowType)
                service.setContextWindowType(contextWindowType);

            service.start(port, loopbackOnly);
        }
    },

    helpInfo: ['-repl              Start REPL.\n',
               '-repl-context      Start in the context gives as window type (see XUL windowtype attribute).\n'].join('')
            
};

/**
* XPCOMUtils.generateNSGetFactory was introduced in Mozilla 2 (Firefox 4).
* XPCOMUtils.generateNSGetModule is for Mozilla 1.9.2 (Firefox 3.6).
*/
if (XPCOMUtils.generateNSGetFactory)
    var NSGetFactory = XPCOMUtils.generateNSGetFactory([MozReplCommandLineHandler]);
else
    var NSGetModule = XPCOMUtils.generateNSGetModule([MozReplCommandLineHandler]);
