

#' @importFrom stats aggregate anova as.formula binomial glm lm lm.wfit pchisq pf quantile density dnorm ecdf loess pnorm predict reorder smooth var
#' @importFrom ggplot2 ggplot_build aes aes_string
#' @importFrom stats median
#' @importFrom wrapr DebugPrintFn let
NULL


#' Check arguments are good for plotting and narrow data frame to only named columns.
#'
#' @param ... should be empty, force later arguments to bind by name.
#' @param frame data.frame to work with.
#' @param name_var_list named list, mapping expected columns to column names used by user.
#' @param title character title for plot.
#' @param funname name of function to use in error messages.
#' @return narrowed data.frame
#'
#' @noRd
check_frame_args_list <- function(...,
                            frame, name_var_list, title,
                            funname = "WVPlots") {
  wrapr::stop_if_dot_args(substitute(list(...)), funname)
  frame_name <- deparse(substitute(frame))
  xvar_name <- deparse(substitute(xvar))
  yvar_name <- deparse(substitute(yvar))
  title_name <- deparse(substitute(title))
  if(missing(frame)||(!is.data.frame(frame))||(nrow(frame)<0)||(ncol(frame)<=0)) {
    stop(paste0(funname, ": data.frame argument ", frame_name, " must be a non-empty data frame"))
  }
  if(missing(title)||(!is.character(title))||(length(title)!=1)) {
    stop(paste0(funname, ": argument ", title_name, " must be set and a length 1 character vector"))
  }
  for(ni in names(name_var_list)) {
    vi <- name_var_list[[ni]]
    if((!is.character(vi))||(length(vi)!=1)) {
      stop(paste0(funname, ": ", ni, " argument must be set and a length 1 character vector"))
    }
    if(!(vi %in% colnames(frame))) {
      stop(paste0(funname, ": ", ni, " argument (value: \"", vi ,"\") must be the name of a column in data.frame ", frame_name))
    }
  }
  # return frame narrowed to named columns
  as.data.frame(frame[, as.character(name_var_list), drop = FALSE])
}


# Curry without leaking
padToK <- function(k) {
  force(k)
  function(x) {
    xres <- as.character(x)
    xres[is.na(xres)] <- 'NA'
    nx <- length(x)
    for(i in seq_len(nx)) {
      di <- k - nchar(xres[[i]])
      if(di>0) {
        pi <- paste(rep('_',di), collapse = '')
        xres[[i]] <- paste(pi,xres[[i]])
      }
    }
    xres
  }
}

# get the y lables from a ready to go ggplot2
getYLabs <- function(p) {
  info <- ggplot2::ggplot_build(p)
  origlabs <- NULL
  tryCatch(
    # ggplot2 version 2.2.1.9000
    origlabs <- info$layout$panel_params[[1]]$y.label,
    error = function(e) {NULL}
  )
  if(!is.null(origlabs)) {
    return(origlabs)
  }
  tryCatch(
    # ggplot2 version 2.2.0
    origlabs <- info$layout$panel_ranges[[1]]$y.labels,
    error = function(e) {NULL}
  )
  if(!is.null(origlabs)) {
    return(origlabs)
  }
  tryCatch(
    # ggplot2 prior to version 2.2.0
    origlabs <- info$panel$ranges[[1]]$y.labels,
    error = function(e) {NULL}
  )
  origlabs
}

# get the x lables from a ready to go ggplot2
getXLabs <- function(p) {
  info <- ggplot2::ggplot_build(p)
  origlabs <- NULL
  tryCatch(
    # ggplot2 version 2.2.1.9000
    origlabs <- info$layout$panel_params[[1]]$x.label,
    error = function(e) {NULL}
  )
  if(!is.null(origlabs)) {
    return(origlabs)
  }
  tryCatch(
    # ggplot2 version 2.2.0
    origlabs <- info$layout$panel_ranges[[1]]$x.labels,
    error = function(e) {NULL}
  )
  if(!is.null(origlabs)) {
    return(origlabs)
  }
  tryCatch(
    # ggplot2 prior to version 2.2.0
    origlabs <- info$panel$ranges[[1]]$x.labels,
    error = function(e) {NULL}
  )
  origlabs
}

# assumes p1 and p2 have not set scale_y_*
designYLabelPadFunction <- function(p1,p2) {
  origlabs1 <- getYLabs(p1)
  origlabs2 <- getYLabs(p2)
  lengthTarget <- max(nchar(c(origlabs1,origlabs2)))
  padToK(lengthTarget)
}


cdeviance <- function(truth,pred,epsilon=1.e-6) {
  pred = pmax(pred, epsilon)
  pred = pmin(pred, 1-epsilon)
  S = 0.0 # assumed log-likelihood of saturated model
  -2*(sum(ifelse(truth,log(pred),log(1-pred)))-S)
}

# this function counts logical as discrete
isDiscrete = function(x) {
  return (is.factor(x) || is.character(x) || is.integer(x) || is.logical(x))
}


