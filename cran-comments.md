

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.3.4.tar.gz
    ...
    * checking CRAN incoming feasibility ... [4s/29s] NOTE
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    
    Found the following (possibly) invalid URLs:
      URL: https://journals.sagepub.com/doi/abs/10.1177/0272989X15582210
        From: man/ROCPlot.Rd
        Status: 403
        Message: Forbidden
    ...
    * checking HTML version of manual ... NOTE
    Found the following HTML validation problems
    ...
    Status: 2 NOTEs


### Windows

  devtools::check_win_devel()

### Linux

  rhub::check_for_cran()
  708#> * using R Under development (unstable) (2023-07-21 r84722 ucrt)
  709#> * using platform: x86_64-w64-mingw32
  710#> * R was compiled by
  711#> gcc.exe (GCC) 12.2.0
  712#> GNU Fortran (GCC) 12.2.0
  720#> Found the following (possibly) invalid URLs:
  721#> URL: https://journals.sagepub.com/doi/abs/10.1177/0272989X15582210
  722#> From: man/ROCPlot.Rd
  723#> Status: 403
  724#> Message: Forbidden
  725#> Maintainer: 'John Mount '
  781#> Found the following files/directories:
  782#> ''NULL''
  783#> * checking for detritus in the temp directory ... NOTE
  784#> Found the following files/directories:
  785#> 'lastMiKTeXException'
  786#> * DONE
  787#> Status: 3 NOTEs

## Downstream dependencies

No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

## Notes

Link https://journals.sagepub.com/doi/abs/10.1177/0272989X15582210 is valid and working (false error detection).
Zumel is not a mis-spelling.
