
Harden package for R4.0.0 defaulting to stringAsFactors=FALSE.

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.5.tar.gz 
    * using R version 4.0.0 (2020-04-24)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.5’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
 
### Windows

    rhub::check_for_cran()
    
    645#> * using R Under development (unstable) (2020-04-22 r78281)
    646#> * using platform: x86_64-w64-mingw32 (64-bit)
    630#> Error in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]) :
    631#> there is no package called 'data.table'


    devtools::check_win_devel()

    Not run at this time.

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
