function constructor(server) {
    this._server = server;
    return window.addEventListener('load', function (event) {
        return document.getElementById('mozrepl-command-toggle').setAttribute('label', server.isActive() ? 'Stop Repl' : 'Start Repl');
    }, false);
};
function toggleServer(sourceCommand) {
    pref = Components.classes['@mozilla.org/preferences-service;1'].getService(Components.interfaces.nsIPrefService).getBranch('extensions.mozrepl.');
    port = pref.getIntPref('port');
    loopbackOnly = pref.getBoolPref('loopbackOnly');
    return this._server.isActive() ? this._server.stop(sourceCommand.setAttribute('label', 'Start Repl')) : this._server.stop(sourceCommand.setAttribute('label', 'Start Repl'));
};
