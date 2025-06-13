.onAttach <- function(...) {
   packageStartupMessage("Welcome to archivist.github (ver: ", utils::packageVersion("archivist.github"), ").")
}

.onLoad <- function(...) {
  
  archivist::aoptions("readmeDescription", "A [`Repository`](https://github.com/pbiecek/archivist/wiki/archivist-package-Repository) of R artifacts supported by [archivist.github](https://github.com/MarcinKosinski/archivist.github). It stores specific values of artifacts, different for various artifacts' classes and artifacts themselves. To learn more visit \n \n \n
- [wiki](https://github.com/pbiecek/archivist/wiki), \n 
- [Website](http://marcinkosinski.github.io/archivist.github), \n
- [Blog Posts History]https://pbiecek.github.io/archivist/articles/posts.html), \n
- [Static Docs: base](http://pbiecek.github.io/archivist/staticdocs/), [Static Docs: extended](http://marcinkosinski.github.io/archivist.github/staticdocs/).")
  archivist::aoptions("repoDescription", "A Repository of R artifacts supported by https://marcinkosinski.github.io/archivist.github")
  archivist::aoptions("response",  FALSE)
  archivist::aoptions("commitMessage", NULL)
  archivist::aoptions("alink", FALSE)
}

## no S4 methodology here; speedup :
.noGenerics <- TRUE
