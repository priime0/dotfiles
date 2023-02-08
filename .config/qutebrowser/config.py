from qutebrowser.api import interceptor
from typing import Dict

config.load_autoconfig()

config.bind("<Shift-j>", "tab-prev")
config.bind("<Shift-k>", "tab-next")
config.bind("<Control-j>", "tab-prev")
config.bind("<Control-k>", "tab-next")

c.content.blocking.method = "both"
c.content.blocking.adblock.lists = [
    "https://easylist.to/easylist/easylist.txt",
    "https://easylist.to/easylist/easyprivacy.txt",
    "https://easylist-downloads.adblockplus.org/antiadblockfilters.txt",
]

c.hints.chars = "aoeuidhtns"


def redirect(request: interceptor.Request):
    redirect_dict: Dict[str, str] = {
        "www.reddit.com": "r.nf",
        "old.reddit.com": "r.nf",
        "reddit.com": "r.nf",
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
