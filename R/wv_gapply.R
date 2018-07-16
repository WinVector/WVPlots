




#' grouped ordered apply (copied and cut down from replyr)
#'
#' Partitions from by values in grouping column, applies a generic transform
#' to each group and then binds the groups back together.  Only advised for a
#' moderate number of groups and better if grouping column is an index.
#' This is powerful
#' enough to implement "The Split-Apply-Combine Strategy for Data Analysis"
#' https://www.jstatsoft.org/article/view/v040i01
#'
#'
#'
#' Note this is a fairly expensive operator, so it only makes sense to use
#' in situations where \code{f} itself is fairly complicated and/or expensive.
#'
#'
#' @param df data.frame
#' @param gcolumn grouping column
#' @param f transform function or pipeline
#' @param ... force later values to be bound by name
#' @param ocolumn ordering column (optional)
#' @param decreasing logical, if TRUE sort in decreasing order by ocolumn
#' @param partitionMethod must be 'split' (only works over local data frames)
#' @param bindrows logical, if TRUE bind the rows back into a data item, else return split list
#' @param eagerCompute logical, if TRUE call compute on split results
#' @param restoreGroup logical, if TRUE restore group column after apply when \code{partitionMethod \%in\% c('extract', 'split')}
#' @return transformed frame
#'
#' @noRd
wv_gapply <- function(df,gcolumn,f,
                      ...,
                      ocolumn=NULL,
                      decreasing=FALSE,
                      partitionMethod='split',
                      bindrows=TRUE,
                      eagerCompute=FALSE,
                      restoreGroup=FALSE) {
  if(!is.data.frame(df)) {
    stop('WVPlots::wv_gapply df must be a data.frame')
  }
  if((!is.character(gcolumn))||(length(gcolumn)!=1)||(nchar(gcolumn)<1)) {
    stop('WVPlots::wv_gapply gcolumn must be a single non-empty string')
  }
  if(!is.null(ocolumn)) {
    if((!is.character(ocolumn))||(length(ocolumn)!=1)||(nchar(ocolumn)<1)) {
      stop('WVPlots::wv_gapply ocolumn must be a single non-empty string or NULL')
    }
  }
  wrapr::stop_if_dot_args(substitute(list(...)), "WVPlots::wv_gapply")
  if(partitionMethod!="split") {
    stop('WVPlots::wv_gapply partitionMethod must be split')
  }
  # only works on local data frames
  base::split(df, df[[gcolumn]]) -> res
  if(!is.null(ocolumn)) {
    orderer <- function(di) {
      if(decreasing) {
        di[rev(order(di[[ocolumn]])), , drop = FALSE]
      } else {
        di[order(di[[ocolumn]]), , drop = FALSE]
      }
    }
    res <- lapply(res, orderer)
  }
  if(!is.null(f)) {
    res <- lapply(res,f)
  }
  if(restoreGroup) {
    res <- lapply(names(res),
                  function(gi) {
                    ri <- res[[gi]]
                    ri[[gcolumn]] <- gi
                    ri
                  }
    )
  }
  if(bindrows) {
    res <- do.call("rbind", res)
  }
  return(res)
}

