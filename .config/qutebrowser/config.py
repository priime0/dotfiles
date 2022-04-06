config.load_autoconfig()

config.bind('<Shift-j>', 'tab-prev')
config.bind('<Shift-k>', 'tab-next')
config.bind('<Control-j>', 'tab-prev')
config.bind('<Control-k>', 'tab-next')

c.content.blocking.method = 'both'
c.content.blocking.adblock.lists = [
    'https://easylist.to/easylist/easylist.txt',
    'https://easylist.to/easylist/easyprivacy.txt',
    'https://easylist-downloads.adblockplus.org/antiadblockfilters.txt'
]

c.hints.chars = 'aoeuidhtns'
