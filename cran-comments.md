
## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.0.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.0’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
 
 
### Windows

    rhub::check_for_cran()
    720#> * using R Under development (unstable) (2019-09-18 r77193)
    721#> * using platform: x86_64-w64-mingw32 (64-bit)
    722#> * using session charset: ISO8859-1
    723#> * using option '--as-cran'
    724#> * checking for file 'WVPlots/DESCRIPTION' ... OK
    725#> * checking extension type ... Package
    726#> * this is package 'WVPlots' version '1.2.0'
    727#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    728#> Maintainer: 'John Mount '
    784#> Status: OK

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).


Zumel is not a mis-spelling.
