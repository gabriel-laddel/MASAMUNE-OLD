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
}

function docFor(thing) {
    var printout = '';
    printout += 'TYPE: ' + (typeof(thing)) + '\n';
    if(thing.name)
        printout += 'NAME: ' + thing.name + '\n';
    else if(thing.nodeName)
        printout += 'NODENAME: ' + thing.nodeName + '\n';

    if(typeof(thing) == 'function') {
        var list = argList(thing);
        printout += 'ARGS: ' + (list.length == 0 ?
                                '[none declared]' :
                                list.join(', ')) + '\n';
    }

    if(thing.doc && typeof(thing.doc) == 'string')
        printout += '\n' + thing.doc + '\n';

    return printout;
}

function argList(fn) {
    var match;
    var rx = new RegExp('^function (\\w+)?\\(([^\\)]*)?\\) {');

    match = fn.toString().match(rx);
    if(match[2])
        return match[2].split(', ');
    else
        return [];
}
