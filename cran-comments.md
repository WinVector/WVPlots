

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.3.2.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.3.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK


### Windows

    devtools::check_win_devel()
    Error in curl::curl_fetch_memory(url, handle = h) : 
      Timeout was reached: [win-builder.r-project.org] FTP response timeout

    rhub::check_for_cran()
     854#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     855#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     856#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
     857#> setting R_REMOTES_STANDALONE to true
     858#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     859#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     860#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     861#> * using log directory 'C:/Users/USERtuXsbaQdWo/WVPlots.Rcheck'
     862#> * using R Under development (unstable) (2020-12-14 r79633)
     863#> * using platform: x86_64-w64-mingw32 (64-bit)
     864#> * using session charset: ISO8859-1
     865#> * using option '--as-cran'
     866#> * checking extension type ... Package
     867#> * this is package 'WVPlots' version '1.3.2'
     868#> * checking for file 'WVPlots/DESCRIPTION' ... OK
     869#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     870#> Maintainer: 'John Mount '
     871#> * checking package namespace information ... OK
     872#> * checking package dependencies ... NOTE
     873#> Suggests orphaned package: 'plotly'
     ...
     927#> Status: 1 NOTE
     We hope plotly will move out of orphaned status soon,
     if not we will later remove the suggestion.

## Downstream dependencies

  No declared reverse dependencies (please see 
  https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
