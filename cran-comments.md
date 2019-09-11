
## Test environments


### OSX

    R CMD check --as-cran WVPlots_1.1.2.tar.gz 
    * using R version 3.6.1 (2019-07-05)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.1.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    * checking package namespace information ... OK
    * checking package dependencies ... OK
    Status: OK
 
 
### Windows

    devtools::check_win_devel()
    
    rhub::check_for_cran()
    723#> * using R Under development (unstable) (2019-08-30 r77101)
    724#> * using platform: x86_64-w64-mingw32 (64-bit)
    725#> * using session charset: ISO8859-1
    726#> * using option '--as-cran'
    727#> * checking for file 'WVPlots/DESCRIPTION' ... OK
    728#> * checking extension type ... Package
    729#> * this is package 'WVPlots' version '1.1.2'
    730#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    731#> Maintainer: 'John Mount '
    787#> Status: OK


## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).


Zumel is not a mis-spelling.
