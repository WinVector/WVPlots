

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.3.3.tar.gz
    * using log directory ‘/Users/johnmount/Documents/work/WVPlots.Rcheck’
    * using R version 4.2.2 (2022-10-31)
    * using platform: x86_64-apple-darwin17.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran
    ...
    Status: OK


### Windows

    devtools::check_win_devel()
    * using log directory 'd:/RCompile/CRANguest/R-devel/WVPlots.Rcheck'
    * using R version 4.3.0 RC (2023-04-13 r84256 ucrt)
    * using platform: x86_64-w64-mingw32 (64-bit)
    ...
    Maintainer: 'John Mount <jmount@win-vector.com>'
    Found the following (possibly) invalid URLs:
      URL: https://journals.sagepub.com/doi/abs/10.1177/0272989X15582210
        From: man/ROCPlot.Rd
        Status: 403
        Message: Forbidden
    ...
    Status: 1 NOTE
    URL check is a false positive. URL loads fine for user browser.

### Linux

  rhub::check_for_cran()
  Build ID:	WVPlots_1.3.3.tar.gz-6aef1fc1ebbd4b2c8c547043dc977338
  Platform:	Windows Server 2022, R-devel, 64 bit
  Submitted:	17 minutes 42.8 seconds ago
  Build time:	6 minutes 55.6 seconds
  NOTES:
  * checking CRAN incoming feasibility ... [15s] NOTE
  Found the following (possibly) invalid URLs:
    URL: https://journals.sagepub.com/doi/abs/10.1177/0272989X15582210
      From: man/ROCPlot.Rd
      Status: 403
      Message: Forbidden
  Maintainer: 'John Mount <jmount@win-vector.com>'
  * checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
  URL is loadable, both the URL check and the lastMiKTeXException are presumably factors of the test environment, not the package.


## Downstream dependencies

  No declared reverse dependencies (please see 
  https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
