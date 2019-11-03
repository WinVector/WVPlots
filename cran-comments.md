
## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.2.tar.gz 
    * using R version 3.6.0 (2019-04-26)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)
    * using session charset: UTF-8
    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘1.2.2’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’
    Status: OK
 
### Windows

    rhub::check_for_cran()
    (service broken)
    
    devtools::check_win_devel()
    (not yet run)
    
### Linux

    rhub::check_for_cran()
    5691#> * using R version 3.6.1 (2019-07-05)
    5692#> * using platform: x86_64-pc-linux-gnu (64-bit)
    5693#> * using session charset: UTF-8
    5694#> * using option ‘--as-cran’
    5695#> * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    5696#> * checking extension type ... Package
    5697#> * this is package ‘WVPlots’ version ‘1.2.2’
    5698#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    5699#> Maintainer: ‘John Mount ’
    5754#> Status: OK

    rhub::check_for_cran()
    2766#> About to run xvfb-run R CMD check --as-cran WVPlots_1.2.2.tar.gz
    2767#> * using log directory ‘/home/docker/WVPlots.Rcheck’
    2768#> * using R Under development (unstable) (2019-11-03 r77361)
    2769#> * using platform: x86_64-pc-linux-gnu (64-bit)
    2770#> * using session charset: UTF-8
    2771#> * using option ‘--as-cran’
    2772#> * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    2773#> * checking extension type ... Package
    2774#> * this is package ‘WVPlots’ version ‘1.2.2’
    2775#> * checking CRAN incoming feasibility ...NB: need Internet access to use CRAN incoming checks
    2776#> Note_to_CRAN_maintainers
    2777#> Maintainer: ‘John Mount ’
    2834#> Status: OK

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
