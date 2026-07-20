# managelocalrepo

This package will allow easier maintainence of CRAN-like repos on local 
networks (i.e. not on CRAN). This might be necessary where hosted packages 
contain intellectual property owned by a corporation that cannot be hosted 
publicly.

## Installation

Easy:

```R
install.packages("managelocalrepo")
```

## Process

1. Set up a directory on your network. This will be your base directory.
2. Create a subdirectory with the name `submissions`. This is where package
files should be put before being released to your local repo.
3. Add the `managelocalrepo.submission` and `managelocalrepo.base` options 
   to your `.First()` file in your `.Rprofile`. For example, your `.Rprofile` 
   might look something like this:

   ```R
   .First <- function () 
   {
       options( 
         repos = c(CORPREPO = "file:////server/file/path/to/your/dir"),
         managelocalrepo.submissions = '/path/to/your/dir/submissions',
         managelocalrepo.base = '/path/to/your/dir')
   }
  ```
  
  The `repos` option will make sure you can install from the corporate repo. 
  The other two options will help point `managelocalrepo` to where it might
  find the package file (e.g. `zip`, `tgz` file) when you use 
  `quick_release_package()`.

Then you can build package file (e.g. `package_file.zip`), have them copied to
the `submissions` folder and then run
`quick_release_package('package_file.zip')`.

## Notes

I've tested this on Windows and OSX. But hopefully it should work on other 
platforms.
