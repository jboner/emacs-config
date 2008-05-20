;;; PYTHON STUFF ---------------------------------------------
(autoload 'python-mode "python-mode" "Python hacking mode." t)
(add-hook 'python-mode-hook
  (lambda ()
    (snippet-with-abbrev-table 'python-mode-abbrev-table
      ("openfr"  .  "open($${file_path}, 'w')$.")
      ("for" .  "for $${element} in $${sequence}:")
      ("im"  .  "import $$")
      ("if"  .  "if $${True}:")
      ("wh"  .  "while $${True}:"))))

