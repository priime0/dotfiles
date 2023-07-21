local autopairs = require("nvim-autopairs")

autopairs.remove_rule("'")
autopairs.remove_rule("(")
autopairs.remove_rule("[")
autopairs.remove_rule("{")
autopairs.remove_rule("<")

vim.o["lisp"] = true
vim.o["autoindent"] = true
