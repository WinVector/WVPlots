

doubleDensity <- function(frame, xvar, truthVar,title='double density plot') {
  ggplot(frame, aes_string(x=xvar, color=truthVar)) +
    geom_density(adjust=0.5,trim=TRUE)
}
