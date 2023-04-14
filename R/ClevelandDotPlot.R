
# move this to sharedFunctions if someone else needs it
isScalar = function(x) {
  return (is.numeric(x) && length(x)==1)
}



stemdotstats = function(ycol) {
  data.frame(y=length(ycol),
             ymax=length(ycol),
             ymin=0)
}

#' Plot a Cleveland dot plot.
#'
#' Plot counts of a categorical variable.
#'
#' Assumes that \code{xvar} is a factor or can be coerced to one (character or integral).
#' \itemize{
#' \item sort < 0 sorts the factor levels in decreasing order (most frequent level first)
#' \item sort > 0 sorts the factor levels in increasing order (good when used in conjunction with coord_flip())
#' \item sort = 0 leaves the factor levels in "natural order" -- usually alphabetical
#' \item stem = FALSE will plot only the dots, without the stem to the y=0 line.
#' \item limit_n = NULL plots all the levels, N an integer limits to the top N most populous levels
#' }
#'
#' @param frm data frame to get values from
#' @param xvar name of the independent (input or model) column in frame
#' @param title title to place on plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param sort if TRUE sort data
#' @param limit_n if not NULL number of items to plot
#' @param stem if TRUE add stems/whiskers to plot
#' @param color color for points and stems
#' @examples
#'
#' set.seed(34903490)
#' # discrete variable: letters of the alphabet
#' # frequencies of letters in English
#' # source: http://en.algoritmy.net/article/40379/Letter-frequency-English
#' letterFreqs = c(8.167, 1.492, 2.782, 4.253, 12.702, 2.228,
#'                 2.015, 6.094, 6.966, 0.153, 0.772, 4.025, 2.406, 6.749, 7.507, 1.929,
#'                 0.095, 5.987, 6.327, 9.056, 2.758, 0.978, 2.360, 0.150, 1.974, 0.074)
#' letterFreqs = letterFreqs/100
#' letterFrame = data.frame(letter = letters, freq=letterFreqs)
#' # now let's generate letters according to their letter frequencies
#' N = 1000
#' randomDraws = data.frame(draw=1:N,
#'   letter=sample(letterFrame$letter, size=N,
#'   replace=TRUE, prob=letterFrame$freq))
#' WVPlots::ClevelandDotPlot(randomDraws, "letter",
#'   title = "Example Cleveland-style dot plot")
#'
#' # Note the use of sort = 0. Also note that the graph omits counts
#' # with no occurrences (5, and 7)
#' WVPlots::ClevelandDotPlot(mtcars, "carb", sort = 0, "Example of counting integer values")
#'
#' # For counting integer values while including counts with no occurrences,
#' # use Discrete Distribution.
#' WVPlots::DiscreteDistribution(mtcars, "carb", "Better way to count integer values")
#' @export
ClevelandDotPlot = function(frm, xvar, title, ...,
                            sort=-1, limit_n = NULL, stem=TRUE,
                            color='black') {
  frm <- as.data.frame(frm)
  check_frame_args_list(...,
                        frame = frm,
                        name_var_list = list(xvar = xvar),
                        title = title,
                        funname = "WVPlots::ClevelandDotPlot")
  if(!(is.null(limit_n) || isScalar(limit_n))) {
    stop("parameter limit_n must either be null or a numeric scalar")
  }
  if(isScalar(limit_n) && (limit_n < 1)) {
    stop("parameter limit_n must be at least 1")
  }

  if(!is.factor(frm[[xvar]])) {
    frm[[xvar]] <- as.factor(frm[[xvar]])
  }

  # to get the top N, we always use decreasing sort order
  if(!is.null(limit_n)) {
    tab = table(frm[[xvar]])
    levelnames = names(tab)
    ord = order(tab, decreasing=TRUE)
    N = min(c(limit_n, length(tab)))
    topN = levelnames[ord][1:N]

    frm = subset(frm, frm[[xvar]] %in% topN)
    frm[[xvar]] = droplevels(frm[[xvar]])
  }

  if(abs(sort) > 0) {
    n = length(frm[[xvar]])
    frm[[xvar]] = reorder(frm[[xvar]], numeric(n)+sort, FUN=sum)
  }
  frm$count = 1
  if(stem) {
    p = ggplot2::ggplot(data = frm, mapping = ggplot2::aes(!!!simulate_aes_string(x=xvar, y="count"))) +
      ggplot2::stat_summary(fun.data=stemdotstats, geom="pointrange", color=color)
  } else {
    p = ggplot2::ggplot(data = frm, mapping = ggplot2::aes(!!!simulate_aes_string(x=xvar))) +
      ggplot2::geom_point(stat="count", color=color)
  }
  p + ggplot2::ggtitle(title)
}
