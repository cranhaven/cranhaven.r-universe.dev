### TukeyC

This is the development code of the R package **TukeyC**.
You should use it if you want to contribute to its development:
testing unreleased versions, fixing bugs, writing code, etc.

To download, check and build it do the following in a terminal emulator:
> git clone  git://github.com/jcfaria/TukeyC.git

> or

> git clone https://jcfaria@github.com/jcfaria/TukeyC.git

After to clone it, to check, build and install do the following:
> R CMD check TukeyC

> R CMD build TukeyC

> R CMD INSTALL TukeyC_X.X-X.tar.gz

Or, you can install using devtools package as:

> library(devtools)

> install_github('jcfaria/TukeyC')
