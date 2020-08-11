

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.6.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.6’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK
 
### Windows

    rhub::check_for_cran()
    891#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    892#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    893#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    894#> setting R_REMOTES_STANDALONE to true
    895#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    896#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    897#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    898#> * using log directory 'C:/Users/USEREfAgOrpWty/WVPlots.Rcheck'
    899#> * using R Under development (unstable) (2020-07-05 r78784)
    900#> * using platform: x86_64-w64-mingw32 (64-bit)
    901#> * using session charset: ISO8859-1
    902#> * using option '--as-cran'
    903#> * checking for file 'WVPlots/DESCRIPTION' ... OK
    904#> * checking extension type ... Package
    905#> * this is package 'WVPlots' version '1.2.6'
    906#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    907#> Maintainer: 'John Mount '
    ...
    963#> Status: OK

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
