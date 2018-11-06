
## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.0.6.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.0.6’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

 
### Windows

    devtools::build_win()
    * using R Under development (unstable) (2018-11-05 r75543)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'WVPlots/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'WVPlots' version '1.0.6'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK
 
## Linux

    rhub::check_for_cran()


## Downstream dependencies

  No declared reverse dependencies:

    devtools::revdep('WVPlots')
    character(0)


Zumel is not a mis-spelling.
