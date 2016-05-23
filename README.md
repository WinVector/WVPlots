# WVPlots

Common plots we use for analysis and presentation (on top of ggplot2 in [R](https://cran.r-project.org)).  For an introduction see: http://www.win-vector.com/blog/2016/04/wvplots-example-plots-in-r-using-ggplot2/

Website: [https://github.com/WinVector/WVPlots](https://github.com/WinVector/WVPlots)

More:

 * http://www.win-vector.com/blog/2013/02/revisiting-clevelands-the-elements-of-graphing-data-in-ggplot2/
 * http://www.win-vector.com/blog/2011/12/my-favorite-graphs/
 * http://www.win-vector.com/blog/2009/11/i-dont-think-that-means-what-you-think-it-means-statistics-to-english-translation-part-1-accuracy-measures/
 

To install in R:

    # install.packages(c('devtools','ggplot2'))
    devtools::install_github('WinVector/WVPlots',build_vignettes=TRUE)


---------------

Some users have been having a bit of trouble using
[`devtools`](https://cran.r-project.org/package=devtools) to install
[`WVPlots`](https://github.com/WinVector/WVPlots) (announced [here](http://www.win-vector.com/blog/2016/04/wvplots-example-plots-in-r-using-ggplot2/) and used to produce some of the graphs shown [here](http://www.win-vector.com/blog/2016/05/pcr_part1_xonly/)). I thought I would
write a note with a few instructions to help.

These are things you should not have to do often, and things those of us already running <code>R</code> have stumbled through and forgotten about.  These are also the kind of finicky system dependent non-repeatable interactive GUI steps you largely avoid once you have a scriptable system like fully R up and running.


First you will need install (likely admin) privileges on your machine
and a network connection that is not blocking and of cran, RStudio or
Github. Make sure you have up to date copies of both
[R](https://cran.r-project.org) and [RStudio](https://www.rstudio.com).
We have to assume you are somewhat familiar with R and RStudio (so
suggest a tutorial if you are not). Once you have these we will try to
"knit" or render a R markdown document. To do this start RStudio select
`File->"New File"->"R Markdown"` as shown below (menus may be different
on different systems, you will have to look around a bit).
![RStudio1](http://www.win-vector.com/blog/wp-content/uploads/2016/05/RStudio1.png "RStudio1.png")
Then click "OK". Then press the "Knit HTML" button as shown in the next
figure.
![RStudio2](http://www.win-vector.com/blog/wp-content/uploads/2016/05/RStudio2.png "RStudio2.png")
This will ask you to pick a filename to save as (anything ending in
".Rmd" will do). If RStudio asks to install anything let it. In the end
you should get a rendered copy of RStudio's example document. If any of
this doesn't work you can look to [RStudio
documentation](http://rmarkdown.rstudio.com). Assuming the above worked
paste the following commands into RStudio's "Console" window (entering a
"return" after the paste to ensure execution). \[Note any time we say
paste or type, watch out for any errors caused by conversion of normal
machine quotes to insidious smart quotes.\] ``

    install.packages(c('RCurl','ggplot2','tidyr',
                        'devtools','knitr'))

The set of packages you actually need can usually be found by looking at
the `R` you wish to run and looking for any `library()` or `::`
commands. R scripts and worksheets tend not to install packages on their
own as that would be a bit invasive. If the above commands execute
without error (messages and warnings are okay) you can then try the
command below to install `WVPlots`: ``

    devtools::install_github('WinVector/WVPlots',
                            build_vignettes=TRUE)

If the above fails (some Windows users are seeing "curl" errors) it can be a problem with your machine (perhaps permissions, or no curl library installed), network, anti-virus, or firewall software.  If it does fail you can try to install <code>WVPlots</code> yourself by doing the following:

1.  Navigate a web browser to <http://winvector.github.io/WVPlots/>.
2.  From there download the file `WVPlots_0.1.tar.gz`.
3.  In the RStudio "Console" window type:
    <code> install.packages(c('ROCR', 'ggplot2', 'gridExtra', 'mgcv', 'plyr', 'reshape2', 'stringr', 'knitr', 'testthat'))</code> (we are installing the dependencies of <code>WVPlots</code> by hand, the dependencies are found by looking at the WVPLots [DESCRIPTION](https://github.com/WinVector/WVPlots/blob/master/DESCRIPTION) file, and excluding <code>grid</code> as it is part of the base system and doesn't need to be installed).
3.  In the RStudio "Console" window type:
    <code>`install.packages('~/Downloads/WVPlots_0.1.tar.gz',repos=NULL)`</code>
    (replacing `'~/Downloads/WVPlots_0.1.tar.gz'` with wherever you
    downloaded `WVPlots_0.1.tar.gz` to).

If the above worked you can test the `WVPlots` package by typing
`library("WVPlots")`. Now you can try knitting one of our example
worksheets.

1.  Navigate a web browser to
    <https://github.com/WinVector/Examples/blob/master/PCR/XonlyPCA.Rmd>
2.  Download the file `XonlyPCA.Rmd` by right-clicking on the "Raw"
    button (towards the top right).
3.  Rename the downloaded file from `XonlyPCA.Rmd.txt` to
    `XonlyPCA.Rmd`.
4.  In Rstudio use `File->"Open File"` to open `XonlyPCA.Rmd`.
5.  Press the "Knit HTML" button (top middle of the editor pane) and this
    should produced the rendered result.

If this isn't working then something is either not installed or
configured correctly, or something is blocking access (such as
anti-virus software or firewall software). The best thing to do is find
another local `R` user and debug together.

