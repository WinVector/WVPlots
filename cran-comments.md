
Maintenance update.  Try to move out of the way of the following dependency chain:

   httpuv breaks shiny,
   shiny breaks crosstalk,
   crosstalk breaks plotly, 
   plotly breaks WVPlots.

   Also broken on the chain, at least: wdman, binman, listviewer, sf, and RSelenium.


## Test environments

    * OSX
    * using R version 3.4.4 (2018-03-15)
    * using platform: x86_64-apple-darwin15.6.0 (64-bit)

    * Windows
    * using R version 3.5.0 alpha (2018-03-30 r74499)
    * using platform: x86_64-w64-mingw32 (64-bit)

## R CMD check --as-cran WVPlots_0.3.1.tar.gz 

    * using option ‘--as-cran’
    * checking for file ‘WVPlots/DESCRIPTION’ ... OK
    * checking extension type ... Package
    * this is package ‘WVPlots’ version ‘0.3.1’
    * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
    Maintainer: ‘John Mount <jmount@win-vector.com>’


## Downstream dependencies

No declared reverse dependencies:

     devtools::revdep('WVPlots')
     character(0)
