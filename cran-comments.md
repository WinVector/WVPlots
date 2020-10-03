

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.3.0.tar.gz 
    * using R version 4.0.2 (2020-06-22)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.3.0’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    ...
    Status: OK

### Windows

    devtools::check_win_devel()
    Error in curl::curl_fetch_memory(url, handle = h) : 
      Failed FTP upload: 550


    rhub::check_for_cran()
    1273#> setting _R_CHECK_FORCE_SUGGESTS_ to false
    1274#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
    1275#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
    1276#> setting R_REMOTES_STANDALONE to true
    1277#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
    1278#> setting _R_CHECK_FORCE_SUGGESTS_ to true
    1279#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
    1280#> * using log directory 'C:/Users/USERglBVxNtDaC/WVPlots.Rcheck'
    1281#> * using R Under development (unstable) (2020-09-16 r79221)
    1282#> * using platform: x86_64-w64-mingw32 (64-bit)
    1283#> * using session charset: ISO8859-1
    1284#> * using option '--as-cran'
    1285#> * checking for file 'WVPlots/DESCRIPTION' ... OK
    1286#> * checking extension type ... Package
    1287#> * this is package 'WVPlots' version '1.3.0'
    1288#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    1289#> Maintainer: 'John Mount '
    ...
    1345#> Status: OK

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
