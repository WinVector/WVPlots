
## Test environments


### OSX

    R CMD check --as-cran WVPlots_1.0.8.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.0.8’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
 
### Windows

    rhub::check_for_cran()
    643#> * using R Under development (unstable) (2018-12-26 r75909)
    644#> * using platform: x86_64-w64-mingw32 (64-bit)
    645#> * using session charset: ISO8859-1
    646#> * using option '--as-cran'
    647#> * checking for file 'WVPlots/DESCRIPTION' ... OK
    648#> * checking extension type ... Package
    649#> * this is package 'WVPlots' version '1.0.8'
    650#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    651#> * checking package namespace information ... OK
    652#> Maintainer: 'John Mount '
    705#> Status: OK


## Downstream dependencies

  No declared reverse dependencies:

    devtools::revdep('WVPlots')
    character(0)

Zumel is not a mis-spelling.
