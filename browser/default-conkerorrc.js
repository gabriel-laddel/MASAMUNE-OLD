// external editing - http://conkeror.org/ExternalEditing

view_source_use_external_editor = true;
editor_shell_command = "emacsclient";

// this + is ~/mozrepl-conkeror.js neccecary for mozrepl
user_pref('extensions.mozrepl.autoStart', true);
let (mozrepl_init = get_home_directory()) {
    mozrepl_init.appendRelativePath(".mozrepl-conkeror.js");
    session_pref('extensions.mozrepl.initUrl', make_uri(mozrepl_init).spec);
}
// misc
require("new-tabs.js");
homepage = "http://log.bitcoin-assets.com/";
session_pref("layout.spellcheckDefault", 1);
minibuffer_auto_complete_default = true; // auto completion in the minibuffer 
url_completion_use_history = true; // should work since bf05c87405
download_buffer_automatic_open_target=OPEN_NEW_BUFFER_BACKGROUND; // No new window for downloads 
url_completion_use_bookmarks = true;
hints_display_url_panel = true; // display the url before going to it in hints mode 
add_hook("mode_line_hook", mode_line_adder(loading_count_widget), true); // # buffers being loaded
remove_hook("mode_line_hook", mode_line_adder(clock_widget));

// don't close by accident 
add_hook("before_quit_hook", function () { 
var w = get_recent_conkeror_window();
var result = (w == null) || "y" == (yield w.minibuffer.read_single_character_option( $prompt = "Quit Conkeror? (y/n)",
$options = ["y", "n"]));
yield co_return(result); }); 
can_kill_last_buffer = false;

require("clicks-in-new-buffer.js"); //Open Middle-Clicked Links in New Buffers
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND; // Now buffers open in background.

// copy url with C-c u
interactive("copy-url",
        "Copy the current buffer's URL to the clipboard",
        function(I) {
            var text = I.window.buffers.current.document.location.href;
            writeToClipboard(text);
            I.window.minibuffer.message("copied: " + text);
        }
);

// reload conkerorrc with C-c r
interactive("reload-config", "reload conkerorrc",
       function(I) {
          load_rc();
          I.window.minibuffer.message("config reloaded");
       }
);

// open url in new background buffer (I can't think of a good keybinding for this) 
interactive("find-url-new-background-buffer", 
"Open a URL in a new background buffer", 
alternates(follow_new_buffer_background, follow_new_window), 
$browser_object = browser_object_url, $prompt = "Find url");

// use M-y to google current selection in new buffer 
// use M-Y to google current selection in new buffer "double-quoted" 
// [ref: http://www.mozdev.org/pipermail/conkeror/2009-February/001334.html ] 
// (See also "**c" for selecting text) 

interactive("search-clipboard-contents", 
	    "Search in Google the content of the X clipboard (the selected text)", 
	    "find-url",
	    $browser_object= function(I) { 
		return "g "+ read_from_x_primary_selection();
	    }); 

interactive("search-clipboard-contents-doublequoted",
	    "Search in Google the content of the X clipboard (the selected text) as a fixed string", 
	    "find-url", 
	    $browser_object= function(I) { return "g \""+ read_from_x_primary_selection()+"\""; } ); 

define_key(default_global_keymap, "C-c u", "copy-url");
define_key(default_global_keymap, "C-c r", "reload-config");
define_key(default_global_keymap, "C-x f", "follow-new-buffer-background");
define_key(content_buffer_normal_keymap, "C-f", "forward"); 
define_key(content_buffer_normal_keymap, "C-b", "back");
// define_key(content_buffer_normal_keymap, "M-f", "buffer-next"); 
// define_key(content_buffer_normal_keymap, "M-b", "buffer-previous");
define_key(content_buffer_normal_keymap, "M-y", "search-clipboard-contents"); 
define_key(content_buffer_normal_keymap, "M-Y", "search-clipboard-contents-doublequoted");


// selection searches
// function create_selection_search(webjump, key) {
//     interactive(webjump+"-selection-search",
//                 "Search " + webjump + " with selection contents",
//                 "find-url-new-buffer",
// 		$browser_object = function (I) {
//                     return webjump + " " + I.buffer.top_frame.getSelection();});
//     define_key(content_buffer_normal_keymap, key.toUpperCase(), webjump + "-selection-search");

//     interactive("prompted-"+webjump+"-search", null,
//                 function (I) {
//                     var term = yield I.minibuffer.read_url($prompt = "Search "+webjump+":",
//                                                            $initial_value = webjump+" ");
//                     browser_object_follow(I.buffer, FOLLOW_DEFAULT, term);
//                 });
//     define_key(content_buffer_normal_keymap, key, "prompted-" + webjump + "-search");
// }

// create_selection_search("g","l");
// create_selection_search("lucky","/"); // _cool
// create_selection_search("wikipedia","w");
// create_selection_search("dictionary","d");
// create_selection_search("amazon","a");
// create_selection_search("youtube","u");
// create_selection_search("maps","p");
// create_selection_search("mp3","p");
// create_selection_search("torrentz","o");
// 
// Interfacing with webpages that steal your focus
// ============================================================================
// 
// A lot of web designers out there seem to think they are doing us a favor by
// using javascript in their web pages to focus form elements like search
// boxes. For the mouse-oriented web surfer it probably is a favor because it
// saves them a click, but for the keyboard-oriented surfer these tricks can be
// incredibly annoying. Let's be clear about one thing: Mozilla provides no
// perfect solution that can allow Conkeror to do the right thing about
// focus-stealing 100% of the time. In the absence of perfection, we have
// figured out two pretty good hacks to fight back against the annoying web
// designers of the world. One of them works imperfectly for every
// focus-stealing event, and the other works perfectly for most focus-stealing
// events. You can choose which you want to use. The best combination might be
// both.
// 
// Imperfect, 100% of the Time
// ===========================
// 
// Conkeror's block-content-focus-change module watches for focus events, and
// permits or blocks them based on how recently the user hit a key or clicked
// the mouse. If the focus event happened within 20 milliseconds (by default) of
// a keypress or mouse click, it is assumed that the event resulted from that
// input event, and allowed to happen. Otherwise the newly focused element is
// immediately unfocused. To enable this mode, put this in your rc:
// 
// require("block-content-focus-change.js");
// 
// I hinted that the 20 millisecond duration could be configured. Here is how you
// could change it if Conkeror seems to be blocking focuses from your clicks
// (happens on slower computers):
// 
// block_content_focus_change_duration = 40;
// 
// Perfect, Most of the Time 
// =========================
// This is a snippet you can put in your rc. It brutally goes through web pages
// as they download and replaces the "focus" method of form elements with a
// dummy function that does nothing. It never results in a false positive, but
// the one kind of focus event that it cannot deal with is when the focus call
// comes from inline javascript in the web page right after the form element
// that it focuses.

function focusblock (buffer) {
    var s = Components.utils.Sandbox(buffer.top_frame);
    s.document = buffer.document.wrappedJSObject;
    Components.utils.evalInSandbox(
        "(function () {\
            function nothing () {}\
            if (! document.forms)\
                return;\
            for (var i = 0, nforms = document.forms.length; i < nforms; i++) {\
              for (var j = 0, nels = document.forms[i].elements.length; j < nels; j++)\
                document.forms[i].elements[j].focus = nothing;\
            }\
          })();",
        s);
}
add_hook('content_buffer_progress_change_hook', focusblock);
