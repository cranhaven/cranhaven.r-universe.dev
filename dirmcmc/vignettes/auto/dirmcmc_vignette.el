(TeX-add-style-hook
 "dirmcmc_vignette"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("todonotes" "disable")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "amsmath"
    "amsthm"
    "amssymb"
    "graphicx"
    "setspace"
    "verbatim"
    "todonotes")
   (TeX-add-symbols
    "myeq")
   (LaTeX-add-bibliographies
    "alpha"
    "references"))
 :latex)

