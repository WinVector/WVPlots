
Note to CRAN: please consider accepting this (too soon) update as it 
may simplify solving some package problems.

Fix release.  Trying to fix WVPlots CRAN Package Check Results 
by lowering dependencies.  
The previously required package replyr is having trouble with 
CRAN Package Check Results:

> Error in namespaceExport(ns, exports) : 
  undefined exports: Module, Rcpp.package.skeleton, populate, loadRcppModules, setRcppClass, loadRcppClass, loadModule, cppFunction, exposeClass, evalCpp, sourceCpp, compileAttributes, registerPlugin, RcppLdFlags, LdFlags, demangle, sizeof, cpp_object_initializer, cpp_object_dummy, Rcpp.plugin.maker
> ERROR: lazy loading failed for package 'replyr'

I believe the above errors are spurious, as replyr does not use C/C++/Rcpp in any manner
and they are occuring on the two archichtures we direclty checked in the
last submission (r-release-osx-x86_64 and r-devel-windows-ix86+x86_64).
However, these issues seem to be triggering 
"Packages required and available but unsuitable versions" errors for WVPlots.
To try and expedite fixing I have greatly lowered WVPlots dependency pattern
(including removing the dependency on replyr).




## Test environments

    * OSX
    * using R version 3.4.3 (2017-11-30)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * Windows
    * using R Under development (unstable) (2018-03-15 r74413)
    * using platform: x86_64-w64-mingw32 (64-bit)


## R CMD check --as-cran WVPlots_0.3.0.tar.gz 

    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘0.3.0’
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’

    Days since last update: 3

No other notes, warnings, or errors.

Status: 1 NOTE
(days since last update)

## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('WVPlots')
     character(0)
