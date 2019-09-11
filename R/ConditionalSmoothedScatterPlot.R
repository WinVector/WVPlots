

check_align = function(arg) {
  if(!(arg %in% c("center", "left", "right")))
    stop(paste("align must be one of 'center','left', or 'right'"))
}

make_window = function(i, k, n, align="center") {
  switch(align,
         "center" = {halfwin = (k-1)/2
         begin = i-halfwin
         end = i+halfwin
         c(begin, end)},
         "left" = c(i-k+1, i),
         "right" = c(i, i+k-1)
  )
}

square_window_i = function(vec, i, k, align="center") {
  window = make_window(i, k, length(vec), align)
  begin=window[1]
  end=window[2]

  if(begin < 1 || end > length(vec)){
    NA
  }
  else{
    mean(vec[begin:end])
  }
}

smoothing = function(frm, xvar, yvar, k, align) {
  vec = frm[[yvar]]
  vapply(seq_len(length(vec)),
         FUN=function(i) {square_window_i(vec, i, k, align=align)},
         numeric(1))
}


#' Plot a scatter plot with smoothing line.
#'
#' Plot a scatter plot with a smoothing line; the smoothing window is aligned either left, center or right.
#'
#' \code{xvar} is the continuous independent variable and \code{yvar} is the dependent binary variable.
#' Smoothing is by a square window of width \code{k}.
#'
#' If \code{palette} is NULL, and \code{groupvar} is non-NULL, plot colors will be chosen from the default ggplot2 palette.
#' Setting \code{palette} to NULL
#' allows the user to choose a non-Brewer palette, for example with \code{\link[ggplot2]{scale_fill_manual}}.
#'
#' @param frame data frame to get values from
#' @param xvar name of the independent column in frame. Assumed to be regularly spaced
#' @param yvar name of the dependent (output or result to be modeled) column in frame
#' @param groupvar name of the grouping column in frame. Can be NULL for an unconditional plot
#' @param title title for plot
#' @param ...  no unnamed argument, added to force named binding of later arguments.
#' @param k width of smoothing window. Must be odd for a center-aligned plot. Defaults to 3
#' @param align smoothing window alignment: 'center', 'left', or 'right'. Defaults to 'center'
#' @param point_color color of points, when groupvar is NULL
#' @param smooth_color color of smoothing line, when groupvar is NULL
#' @param palette name of Brewer palette, when groupvar is non-NULL (can be NULL)
#' @examples
#'
#' y = c(1,2,3,4,5,10,15,18,20,25)
#' x = seq_len(length(y))
#' df = data.frame(x=x, y=y, group=x>5)
#' WVPlots::ConditionalSmoothedScatterPlot(df, "x", "y", NULL,
#'    title="left smooth, one groups", align="left")
#' WVPlots::ConditionalSmoothedScatterPlot(df, "x", "y", "group",
#'    title="left smooth, two groups", align="left")
#'
#' @export
ConditionalSmoothedScatterPlot = function(frame, xvar, yvar, groupvar, title, ...,
                                          k=3, align="center",
                                          point_color="black", smooth_color="black",
                                          palette="Dark2") {
  vlist <- list(xvar = xvar, yvar = yvar)
  if(!is.null(groupvar)) {
    vlist$groupvar <- groupvar
  }
  frame <- as.data.frame(frame)
  check_frame_args_list(...,
                        frame = frame,
                        name_var_list = vlist,
                        title = title,
                        funname = "WVPlots::ConditionalSmoothedScatterPlot")
  if(!is.null(groupvar)) {
    if(!isDiscrete(frame[[groupvar]])) {
      stop(paste(groupvar, "should be discrete (factor, character, integer, or logical)"))
    }
  }
  check_align(align)
  if((k%%2)==0 && align=="center") {stop("For centered windows, k must be odd")}

  # sort the frame by x
  ord = order(frame[[xvar]])
  frame=frame[ord,]

  fs = frame
  fs$smooth=0

  if(is.null(groupvar)) {
    fs$smooth = smoothing(fs, xvar, yvar, k, align)
    fs = fs[!is.na(fs$smooth),]
    p =  ggplot2::ggplot() +
      ggplot2::geom_point(data=frame, ggplot2::aes_string(x=xvar, y=yvar), color=point_color) +
      ggplot2::geom_line(data=fs, ggplot2::aes_string(x=xvar, y="smooth"), color=smooth_color)
  } else{
    gplist = unique(fs[[groupvar]])
    for(gp in gplist) {
      ix = fs[[groupvar]]==gp
      fs$smooth[ix] = smoothing(fs[ix, ], xvar, yvar, k, align)
    }
    fs = fs[!is.na(fs$smooth),]
    p =  ggplot2::ggplot() +
      ggplot2::geom_point(data=frame, ggplot2::aes_string(x=xvar, y=yvar, color=groupvar)) +
      ggplot2::geom_line(data=fs, ggplot2::aes_string(x=xvar,y="smooth",color=groupvar))

    if(!is.null(palette)) {
      p = p + ggplot2::scale_color_brewer(palette=palette)
    }
  }
  p + ggplot2::ggtitle(title)
}

