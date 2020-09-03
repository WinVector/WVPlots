

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.8.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.8’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    * checking for future file timestamps ... NOTE
    unable to verify current time
    ...
    Status: 1 NOTE
    No idea why the check can not verify current time, suspect it is a services/environment issue- not a package issue.

### Windows

    rhub::check_for_cran()
    ...
    771#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    772#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    773#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    774#> setting R_REMOTES_STANDALONE to true
    775#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    776#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    777#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    778#> * using log directory 'C:/Users/USEROWPNbKTAex/WVPlots.Rcheck'
    779#> * using R Under development (unstable) (2020-07-05 r78784)
    780#> * using platform: x86_64-w64-mingw32 (64-bit)
    781#> * using session charset: ISO8859-1
    782#> * using option '--as-cran'
    783#> * checking for file 'WVPlots/DESCRIPTION' ... OK
    784#> * checking extension type ... Package
    785#> * this is package 'WVPlots' version '1.2.8'
    786#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    787#> Maintainer: 'John Mount '
    ...
    799#> * checking for future file timestamps ... NOTE
    800#> unable to verify current time
    ...
    844#> Status: 1 NOTE
    No idea why the check can not verify current time, suspect it is a services/environment issue- not a package issue.

    devtools::check_win_devel()
    ...
    Error in curl::curl_fetch_memory(url, handle = h) : 
      Timeout was reached: [win-builder.r-project.org] FTP response timeout

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
