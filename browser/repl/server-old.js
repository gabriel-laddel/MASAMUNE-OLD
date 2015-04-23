var port = 4255;
var Cc = Components.classes;
var Ci = Components.interfaces;
var loader = Cc['@mozilla.org/moz/jssubscript-loader;1']
    .getService(Ci.mozIJSSubScriptLoader);
var srvPref = Cc['@mozilla.org/preferences-service;1']
    .getService(Ci.nsIPrefService);
var srvObserver = Cc['@mozilla.org/observer-service;1']
    .getService(Ci.nsIObserverService);
var inputBuffer = '';

function log(msg) {
    dump(msg + '\n');
}

function scan(string, separator) {
    var match = string.match(separator);
    if(match)
        return [string.substring(0, match.index),
                string.substr(match.index + match[0].length)];
    else
        return [null, string];
}

function isTopLevel(object) {
    return (object instanceof Ci.nsIDOMWindow ||
            'wrappedJSObject' in object ||
            'NSGetModule' in object ||
            'EXPORTED_SYMBOLS' in object ||
            (object.__parent__ && 'EXPORTED_SYMBOLS' in object.__parent__));
}

function _generateExposedProps(obj) {
    var props = {};
    Object.keys(obj).filter(function (k) k[0] !== '_').
        forEach(function (k) {
            props[k] = 'r';
        });
    return props;
}

function helpUrlFor(thing) {
    function mdcXpcomClassUrl(classID) {
        return 'https://developer.mozilla.org/en-US/search?q=' + escape('"'+classID+'"');
    }
    function mdcXulElementUrl(element) {
        return 'http://developer.mozilla.org/en/XUL/' +
            element.nodeName;
    }

    if(typeof(thing) == 'string') {
        if(thing.match(/^@mozilla.org\//))
            return mdcXpcomClassUrl(thing);

    } else if(thing.QueryInterface &&
              (function() {
                  var NS_NOINTERFACE = 0x80004002;
                  try {
                      thing.QueryInterface(Components.interfaces.nsIDOMXULElement);
                      return true;
                  } catch(e if e.result == NS_NOINTERFACE) {}
              })()) {
        return mdcXulElementUrl(thing);
    }
};

function docFor(thing) {
    var printout = '';
    printout = 'TYPE: ' + typeof thing + 'n';
    if (thing.name) {
        printout = 'NAME: ' + thing.name + 'n';
    } else {
        if (thing.nodeName) {
            printout = printout + 'NODENAME: ' + thing.nodeName + 'n';
        };
    };
    if (typeof thing == 'function') {
        var list = argList(thing);
        printout += 'ARGS: ' + (list.length == 0 ? '[none declared]' : list.join(', ')) + 'n';
    };
    if (thing.doc && typeof thing.doc == 'string') {
        printout = printout + 'n' + thing.doc + 'n';
    };
    return printout;
};

function doc(thing) {
    print(util.docFor(thing));
    var url = util.helpUrlFor(thing);
    if(url) {
        print('Online help found, displaying...');
        Cc['@mozilla.org/embedcomp/window-watcher;1']
            .getService(Ci.nsIWindowWatcher)
            .openWindow(null, url, 'help',
                        'width=640,height=600,scrollbars=yes,menubars=no,' +
                        'toolbar=no,location=no,status=no,resizable=yes', null);
    }
}
doc.doc = 'Looks up documentation for a given object';

function print(data, appendNewline) {
    var string = data == undefined ?
        '\n' :
        data + (appendNewline == false ? '' : '\n');
}

function represent(thing) {
    var represent = arguments.callee;
    var s;
    switch(typeof(thing)) {
    case 'string':
        s = '"' + thing + '"';
        break;
    case 'number':
        s = thing;
        break;
    case 'object':
        var names = [];
        for(var name in thing)
            names.push(name);

        s = thing;
        if(names.length > 0) {
            s += ' - {';
            s += names.slice(0, 7).map(function(n) {
                var repr = n + ': ';
                try {
                    if(thing[n] === null)
                        repr += 'null';
                    else if(typeof(thing[n]) == 'object')
                        repr += '{...}';
                    else
                        repr += represent(thing[n]);
                } catch(e) {
                    repr += '[Exception!]'
                }
                return repr;
            }).join(', ');
            if(names.length > 7)
                s += ', ...'
            s += '}';
        }
        break;
    case 'function':
        s = 'function() {...}';
        break;
    default:
        s = thing;
    }
    return s;
}

function inspect(obj, maxDepth, name, curDepth) {
    // adapted from ddumpObject() at
    // http://lxr.mozilla.org/mozilla/source/extensions/sroaming/resources/content/transfer/utility.js

    function crop(string, max) {
        string = string.match(/^(.+?)(\n|$)/m)[1];
        max = max || 70;
        return (string.length > max-3) ?
            string.slice(0, max-3) + '...' : string;
    }

    if(name == undefined)
        name = '<' + typeof(obj) + '>';
    if(maxDepth == undefined)
        maxDepth = 0;
    if(curDepth == undefined)
        curDepth = 0;
    if(maxDepth != undefined && curDepth > maxDepth)
        return;

    var i = 0;
    for(var prop in obj) {
        if(obj instanceof Ci.nsIDOMWindow &&
           (prop == 'java' || prop == 'sun' || prop == 'Packages')) {
            print(name + "." + prop + "=[not inspecting, dangerous]");
            continue;
        }

        try {
            i++;
            if(obj[prop] === null)
                print(name + "." + prop + '=null');
            else if(typeof(obj[prop]) == "object") {
                if(obj.length != undefined)
                    print(name + "." + prop + "=[probably array, length "
                               + obj.length + "]");
                else
                    print(name + "." + prop + "=[" + typeof(obj[prop]) + "]");

                this.inspect(obj[prop], maxDepth, name + "." + prop, curDepth+1);
            }
            else if(typeof(obj[prop]) == "function")
                print(name + "." + prop + "=[function]");
            else if(typeof(obj[prop]) == "xml") {
                let s = obj[prop].toXMLString().replace(/>\n\s*/g, ' ');
                print(name + "." + prop + "=" + (s.length > 100 ? s.slice(0, 97) + '...' : s));
            }
            else
                print(name + "." + prop + "=" + obj[prop]);

            if(obj[prop] && obj[prop].doc && typeof(obj[prop].doc) == 'string')
                print('    ' + crop(obj[prop].doc));

        } catch(e) {
            print(name + '.' + prop + ' - Exception while inspecting.');
        }
    }
    if(!i)
        print(name + " is empty");
}

function formatStackTrace(exception) {
    var trace = '';
    if(exception.stack) {
        var calls = exception.stack.split('\n');
        for each(var call in calls) {
            if(call.length > 0) {
                call = call.replace(/\\n/g, '\n');

                if(call.length > 200)
                    call = call.substr(0, 200) + '[...]\n';

                trace += call.replace(/^/mg, '\t') + '\n';
            }
        }
    }
    return trace;
}

function reloadChrome() {
    try {
	Cc["@mozilla.org/chrome/chrome-registry;1"].
            getService(Ci.nsIXULChromeRegistry).reloadChrome();
    } catch(e) { print('Exception while reloading chrome: '+e); }
}
reloadChrome.doc = "Reload all chrome packages";

// function REPL() {
//     // FIX #37 (https://github.com/bard/mozrepl/issues/37)
//     // needed by toolkit >= 17.0
//     // http://blog.mozilla.org/addons/2012/08/20/exposing-objects-to-content-safely/
//     this.__exposedProps__ = this.__exposedProps__ || _generateExposedProps(this.__proto__);
// };
// loader.loadSubScript('file:///root/quicklisp/local-projects/masamune/browser/repl/repl-old.js', 
// 		     REPL.prototype);

var serv;
var contextWindowType;

function onSocketAccepted(serv, transport) {
    try {
        var outstream = transport.openOutputStream(Ci.nsITransport.OPEN_BLOCKING , 0, 0);
        var outstreamutf8 = Cc['@mozilla.org/intl/converter-output-stream;1']
            .createInstance(Ci.nsIConverterOutputStream);
        outstreamutf8.init(outstream, 'UTF-8', 0, 0);

        var instream = transport.openInputStream(0, 0, 0);
        var instreamutf8 = Cc['@mozilla.org/intl/converter-input-stream;1'].createInstance(Ci.nsIConverterInputStream);
        instreamutf8.init(instream, 'UTF-8', 1024, 0);
    } catch(e) {
        log('REPL ERROR: ' + e);
    }

    var context = Cc['@mozilla.org/appshell/window-mediator;1'].getService(Ci.nsIWindowMediator).getMostRecentWindow("") || Cc["@mozilla.org/appshell/appShellService;1"].getService(Ci.nsIAppShellService).hiddenDOMWindow.wrappedJSObject;

    onOutput = function(string) {
        outstreamutf8.writeString(string);
    };
    onQuit = function() {
        log('REPL, Client closed connection: ' + transport.host + ':' + transport.port);        
        instream.close();
        outstream.close();
    };

    var pump = Cc['@mozilla.org/network/input-stream-pump;1'].createInstance(Ci.nsIInputStreamPump);
    pump.init(instream, -1, -1, 0, 0, false);
    pump.asyncRead({
        onStartRequest: function(request, context) {
	    log("REPL start request");
	},
        onStopRequest: function(request, context, status) {
            log("REPL stop request");
        },
        onDataAvailable: function(request, context, inputStream, offset, count) {
            var str = {}
            instreamutf8.readString(count, str);
	    var stringToEval = str.value
	    try {
		var result = eval(stringToEval);
		log('REPL received data: ' + stringToEval);
	    } catch (e) {
		log('REPL attempted to eval ' + stringToEval + ' and got exception ' + e);
	    };
        }
    }, null);
}

// try {
//     var dbgPrefs = ["nglayout.debug.disable_xul_cache", 
//       	            "javascript.options.showInConsole", 
//                     "browser.dom.window.dump.enabled"];

//     var prefs = Cc["@mozilla.org/preferences-service;1"]
//         .getService(Ci.nsIPrefBranch);

//     for each (let pname in dbgPrefs) { 
//         prefs.setBoolPref(pname, enabled); 
//     }
// } catch(e) { 
//     log('Exception while setting debugging preferences: '+ e); 
// }

try {
    serv = Cc['@mozilla.org/network/server-socket;1'].createInstance(Ci.nsIServerSocket);
    serv.init(port, true, -1);
    serv.asyncListen(this);
    log('REPL Listening at: 127.0.0.1: ' + port);
} catch(e) {
    log('REPL Error: ' + e);
}

log("finished loading");
