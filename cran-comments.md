
## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.3.tar.gz
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.3’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
 
### Windows

    rhub::check_for_cran()
     968#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     969#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     970#> setting R_REMOTES_STANDALONE to true
     971#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     972#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     973#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     974#> * using log directory 'C:/Users/USERkZjcMBnuWD/WVPlots.Rcheck'
     975#> * using R Under development (unstable) (2020-01-07 r77637)
     976#> * using platform: x86_64-w64-mingw32 (64-bit)
     977#> * using session charset: ISO8859-1
     978#> * using option '--as-cran'
     979#> * checking for file 'WVPlots/DESCRIPTION' ... OK
     980#> * checking extension type ... Package
     981#> * this is package 'WVPlots' version '1.2.3'
     982#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     983#> Maintainer: 'John Mount '
     1039#> Status: OK

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
