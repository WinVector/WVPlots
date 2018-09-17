
## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.0.3.tar.gz 
    * using R version 3.5.0 (2018-04-23)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.0.3’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK

### Windows

    devtools::buils_win()
    * using R Under development (unstable) (2018-09-16 r75318)
    * using platform: x86_64-w64-mingw32 (64-bit)
    * this is package 'WVPlots' version '1.0.3'
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Status: OK

## Downstream dependencies

  No declared reverse dependencies:

    devtools::revdep('WVPlots')
    character(0)

Zumel is not a mis-spelling.
