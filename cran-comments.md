
## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.0.5.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.0.5’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::build_win()
    * using R Under development (unstable) (2018-10-21 r75476)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * using session charset: ISO8859-1
    * checking for file 'WVPlots/DESCRIPTION' ... OK
    * checking extension type ... Package
    * this is package 'WVPlots' version '1.0.5'
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Found the following (possibly) invalid URLs:
     URL: https://cran.r-project.org
        From: inst/doc/WVPlots_concept.html
              README.md
        Status: Error
        Message: libcurl error code 7:
      	    Failed to connect to cran.r-project.org port 443: Timed out
    Status: 1 NOTE
    URL is good, error message is spurious.
      	
## Linux

    rhub::check_for_cran()
    3207#> * using R version 3.4.4 (2018-03-15)
    3208#> * using platform: x86_64-pc-linux-gnu (64-bit)
    3209#> * using session charset: UTF-8
    3210#> * using option ‘--as-cran’
    3211#> * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    3212#> * checking extension type ... Package
    3213#> * this is package ‘WVPlots’ version ‘1.0.5’
    3214#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    3215#> Maintainer: ‘John Mount ’
    3268#> Status: OK
    

## Downstream dependencies

  No declared reverse dependencies:

    devtools::revdep('WVPlots')
    character(0)

Zumel is not a mis-spelling.
