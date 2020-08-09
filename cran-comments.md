

## Test environments

### OSX

    R CMD check --as-cran WVPlots_1.2.6.tar.gz 

 
### Windows

    devtools::check_win_devel()

    rhub::check_for_cran()
     776#> setting _R_CHECK_FORCE_SUGGESTS_ to false
     777#> setting R_COMPILE_AND_INSTALL_PACKAGES to never
     778#> setting _R_CHECK_THINGS_IN_CHECK_DIR_ to false
     779#> setting R_REMOTES_STANDALONE to true
     780#> setting R_REMOTES_NO_ERRORS_FROM_WARNINGS to true
     781#> setting _R_CHECK_FORCE_SUGGESTS_ to true
     782#> setting _R_CHECK_CRAN_INCOMING_USE_ASPELL_ to true
     783#> * using log directory 'C:/Users/USEROSxpKwjYWG/WVPlots.Rcheck'
     784#> * using R Under development (unstable) (2020-07-05 r78784)
     785#> * using platform: x86_64-w64-mingw32 (64-bit)
     786#> * using session charset: ISO8859-1
     787#> * using option '--as-cran'
     788#> * checking for file 'WVPlots/DESCRIPTION' ... OK
     789#> * checking extension type ... Package
     790#> * this is package 'WVPlots' version '1.2.6'
     791#> * checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
     792#> Maintainer: 'John Mount '
     793#> * checking package namespace information ... OK
     794#> * checking package dependencies ... OK
     795#> * checking if this is a source package ... OK
     796#> * checking if there is a namespace ... OK
     797#> * checking for executable files ... OK
     798#> * checking for hidden files and directories ... OK
     799#> * checking for portable file names ... OK
     800#> * checking serialization versions ... OK
     801#> * checking whether package 'WVPlots' can be installed ... OK
     802#> * checking installed package size ... OK
     803#> * checking package directory ... OK
     804#> * checking for future file timestamps ... OK
     805#> * checking 'build' directory ... OK
     806#> * checking DESCRIPTION meta-information ... OK
     807#> * checking top-level files ... OK
     808#> * checking for left-over files ... OK
     809#> * checking index information ... OK
     810#> * checking package subdirectories ... OK
     811#> * checking R files for non-ASCII characters ... OK
     812#> * checking R files for syntax errors ... OK
     813#> * checking whether the package can be loaded ... OK
     814#> * checking whether the package can be loaded with stated dependencies ... OK
     815#> * checking whether the package can be unloaded cleanly ... OK
     816#> * checking whether the namespace can be loaded with stated dependencies ... OK
     817#> * checking whether the namespace can be unloaded cleanly ... OK
     818#> * checking loading without being on the library search path ... OK
     819#> * checking use of S3 registration ... OK
     820#> * checking dependencies in R code ... OK
     821#> * checking S3 generic/method consistency ... OK
     822#> * checking replacement functions ... OK
     823#> * checking foreign function calls ... OK
     824#> * checking R code for possible problems ... OK
     825#> * checking Rd files ... OK
     826#> * checking Rd metadata ... OK
     827#> * checking Rd line widths ... OK
     828#> * checking Rd cross-references ... WARNING
     829#> '[ggplot2]{scale_fill_manual}'
     830#> Non-file package-anchored link(s) in documentation object 'ConditionalSmoothedScatterPlot.Rd':
     831#> Non-file package-anchored link(s) in documentation object 'DoubleHistogramPlot.Rd':
     832#> '[ggplot2]{scale_fill_manual}'
     833#> Non-file package-anchored link(s) in documentation object 'PairPlot.Rd':
     834#> '[ggplot2]{scale_color_manual}'
     835#> '[ggplot2]{scale_color_manual}'
     836#> Non-file package-anchored link(s) in documentation object 'ROCPlotPair.Rd':
     837#> Non-file package-anchored link(s) in documentation object 'ROCPlotPair2.Rd':
     838#> '[ggplot2]{scale_color_manual}'
     839#> Non-file package-anchored link(s) in documentation object 'ShadowHist.Rd':
     840#> '[ggplot2]{scale_fill_manual}'
     841#> Non-file package-anchored link(s) in documentation object 'ShadowPlot.Rd':
     842#> '[ggplot2]{scale_fill_manual}'
     843#> Non-file package-anchored link(s) in documentation object 'reexports.Rd':
     844#> '[wrapr]{%:=%}' '[wrapr]{%.>%}'
     845#> See section 'Cross-references' in the 'Writing R Extensions' manual.
     846#> '[ggplot2]{scale_fill_manual}'
     847#> Non-file package-anchored link(s) in documentation object 'DoubleDensityPlot.Rd':
     848#> * checking for missing documentation entries ... OK
     849#> * checking for code/documentation mismatches ... OK
     850#> * checking Rd \usage sections ... OK
     851#> * checking Rd contents ... OK
     852#> * checking for unstated dependencies in examples ... OK
     853#> * checking installed files from 'inst/doc' ... OK
     854#> * checking files in 'vignettes' ... OK
     855#> * checking examples ... OK
     856#> * checking for unstated dependencies in 'tests' ... OK
     857#> * checking tests ...
     858#> Running 'package_test_runner.R'
     859#> OK
     860#> * checking for unstated dependencies in vignettes ... OK
     861#> * checking package vignettes in 'inst/doc' ... OK
     862#> * checking re-building of vignette outputs ... OK
     863#> * checking PDF version of manual ... OK
     864#> * checking for non-standard things in the check directory ... OK
     865#> * checking for detritus in the temp directory ... OK
     866#> * DONE
     867#> Status: 1 WARNING
     868#> See
     869#> 'C:/Users/USEROSxpKwjYWG/WVPlots.Rcheck/00check.log'
     870#> for details.

## Downstream dependencies

  No declared reverse dependencies (please see https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
