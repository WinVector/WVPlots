

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
    Error in curl::curl_fetch_memory(url, handle = h) : 
      Timeout was reached: [win-builder.r-project.org] FTP response timeout

    rhub::check_for_cran()


## Downstream dependencies

  No declared reverse dependencies (please see 
  https://github.com/WinVector/WVPlots/blob/master/extras/check_reverse_dependencies.md ).

Zumel is not a mis-spelling.
