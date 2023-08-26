from qutebrowser.api import interceptor
from typing import Dict

config.load_autoconfig()

c.bindings.default["normal"].clear()
c.bindings.commands["normal"] = {
    # Navigation
    "<Control-v>":            "scroll-page 0 0.5",
    "<Alt-v>":                "scroll-page 0 -0.5",
    "<":                      "scroll-to-perc 0",
    ">":                      "scroll-to-perc",

    # Commands
    "<Alt-x>":                "cmd-set-text :",
    "<Control-x>b":           "cmd-set-text :tab-focus",
    "<Control-d>":            "tab-close",
    "<Control-x><Control-c>": "quit",

    # Searching
    "<Control-s>":            "cmd-set-text /",
    "<Control-r>":            "cmd-set-text ?",

    # Hinting
    "<Alt-s>":                "hint all",
    "<Alt-Shift-s>":          "hint all tab",

    # History
    "<Control-C>f":           "forward",
    "<Control-C>p":           "back",

    # Zoom
    "<Control-X>+":           "zoom-in",
    "<Control-X>-":           "zoom-out",

    # Tabs
    "<Shift-right>":          "tab-next",
    "<Shift-left>":           "tab-prev",

    # Open
    "<Control-x><Control-f>": "cmd-set-text -s :open",
    "<Control-x><Control-e>": "cmd-set-text -s :open {url:pretty}",
    "<Control-x><Control-o>": "cmd-set-text -s :open -t",

    # Editing
    "<Control-f>":            "fake-key <Right>",
    "<Control-b>":            "fake-key <Left>",
    "<Control-n>":            "fake-key <Down>",
    "<Control-p>":            "fake-key <Up>",

    # Numbers
    "0":                      "fake-key 0",
    "1":                      "fake-key 1",
    "2":                      "fake-key 2",
    "3":                      "fake-key 3",
    "4":                      "fake-key 4",
    "5":                      "fake-key 5",
    "6":                      "fake-key 6",
    "7":                      "fake-key 7",
    "8":                      "fake-key 8",
    "9":                      "fake-key 9",

    # Links
    "<Alt-w>":                "yank",
}

c.bindings.commands["command"] = {
    # Searching
    "<Control-s>":            "search-next",
    "<Control-r>":            "search-prev",

    # Completion
    "<Control-p>":            "completion-item-focus prev",
    "<Control-n>":            "completion-item-focus next",

    "<Control-g>":            "leave-mode",
}

c.bindings.commands["hint"] = {
    "<Control-g>":            "leave-mode",
}

c.bindings.commands["caret"] = {
    "<Control-g>":            "leave-mode",
}

c.content.blocking.method = "both"
c.content.blocking.adblock.lists = [
    "https://easylist.to/easylist/easylist.txt",
    "https://easylist.to/easylist/easyprivacy.txt",
    "https://easylist-downloads.adblockplus.org/antiadblockfilters.txt",
]

c.hints.chars = "aoeuidhtns"


def redirect(request: interceptor.Request):
    redirect_dict: Dict[str, str] = {
    }

    if request.request_url.host() in redirect_dict.keys():
        try:
            new_host = redirect_dict[request.request_url.host()]
            request.request_url.setHost(new_host)
            request.redirect(request.request_url)
        except:
            pass

    if request.request_url.host() == "news.ycombinator.com":
        try:
            request.request_url.setHost("hn.svelte.dev")
            query = request.request_url.query()
            if len(query) != 0:
                queries = query.split("&")
                for q in queries:
                    if q.startswith("id"):
                        id = q.split("=")[1]
                        request.request_url.setPath(f"/item/{id}")
                        request.request_url.setQuery("")
            request.redirect(request.request_url)
        except:
            pass


interceptor.register(redirect)
