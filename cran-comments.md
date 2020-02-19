
Harden package for R4.0.0 defaulting to stringAsFactors=FALSE.

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.4.tar.gz
    * using R version 3.6.2 (2019-12-12)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.4’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
 
### Windows

    Not run at this time.

### Linux

    rhub::check_for_cran()
    About to run xvfb-run R CMD check --as-cran WVPlots_1.2.4.tar.gz
    * using log directory ‘/home/docker/WVPlots.Rcheck’
    * using R Under development (unstable) (2020-02-16 r77809)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.4’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

    rhub::check_for_cran()
    About to run xvfb-run R CMD check --as-cran WVPlots_1.2.4.tar.gz
    * using log directory ‘/home/docker/WVPlots.Rcheck’
    * using R version 3.6.1 (2019-07-05)
    * using platform: x86_64-pc-linux-gnu (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.4’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
