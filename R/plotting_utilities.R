# Note that most of these come from
#
# http://motioninsocial.com/tufte/



#' Plots a histogram of column i
#'
#' @param data_in The input data frame
#' @param i The index of the column
#'
#' @export plotHist
#'
#' @examples
#'
#' A <- data.frame(x = rep(c("YES","NO"), 10))
#' plotHist(A,1)
#'
plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) +
    stat_count() +
    xlab(colnames(data_in)[i]) +
    theme_tufte(base_size = 12)
  return (p)
}

#' Creates histograms for categorical variables
#'
#' @param data_in The input data frame
#' @param fun Plotting function, i.e. plotHist for categorical variables
#' @param ii Indices of the columns to plot
#' @param ncol Number of columns (can't be 1)
#'
#' @export doPlots
#' @import ggplot2 gridExtra ggthemes
#'
#' @examples
#'
#' A <- data.frame(x = rep(c("YES","NO"), 10),
#'                 y = rep(c('R','G','B','F'), 5),
#'                 z = rep(c("YES","NO"), 10),
#'                 t = rep(c('R','G','B','F'), 5))
#' doPlots(A, fun = plotHist, ii = 1:4, ncol = 2)
#'
doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

#' Dot-dash (or rug) scatterplot
#'
#' As in page 14 of "The Visual Display of Quantitative Information" by E.R. Tufte
#'
#' @param data_in Dataframe in
#' @param xname Name of the column for the X axis
#' @param yname Name of the column for the Y axis
#' @param xlab Title of the X axis, default NULL
#' @param ylab Title of the Y axis, default NULL
#'
#' @import ggthemes ggplot2
#' @export dot_dash_plot
#'
#' @examples
#'
#' dot_dash_plot(iris, "Sepal.Width", "Petal.Length", "Sepal Length" , "Petal Length")
#'
dot_dash_plot <- function(data_in, xname, yname, xlab = NULL, ylab = NULL) {
  ggplot(data_in, aes_string(xname,yname)) +
    geom_point() +
    geom_rug() +
    theme_tufte(ticks=F) +
    xlab(xlab) +
    ylab(ylab) +
    theme(axis.title.x = element_text(vjust=-0.5),
          axis.title.y = element_text(vjust=1))
}


#' Range-frame scatterplot
#'
#' As in page 131 of "The Visual Display of Quantitative Information" by E.R. Tufte
#'
#' @param data_in Dataframe in
#' @param xname Name of the column for the X axis
#' @param yname Name of the column for the Y axis
#' @param xlab Title of the X axis, default NULL
#' @param ylab Title of the Y axis, default NULL
#'
#' @import ggthemes ggplot2
#' @export range_frame_plot
#'
#' @examples
#'
#' range_frame_plot(iris, "Sepal.Width", "Petal.Length", "Sepal Length" , "Petal Length")
#'
range_frame_plot <- function(data_in, xname, yname, xlab = NULL, ylab = NULL) {
  ggplot(data_in, aes_string(xname,yname)) +
    geom_point() +
    geom_rangeframe() +
    theme_tufte() +
    xlab(xlab) +
    ylab(ylab) +
    theme(axis.title.x = element_text(vjust=-0.5),
          axis.title.y = element_text(vjust=1.5))
}


#' Marginal histogram scatterplot
#'
#' As in page 134 of "The Visual Display of Quantitative Information" by E.R. Tufte
#'
#' @param data_in Dataframe in
#' @param xname Name of the column for the X axis
#' @param yname Name of the column for the Y axis
#' @param margin_type Can be "histogram" "density" "violin" or "boxplot"
#' @param xlab Title of the X axis, default NULL
#' @param ylab Title of the Y axis, default NULL
#'
#' @import ggthemes ggplot2 ggExtra
#' @export marginal_histogram_plot
#'
#' @examples
#'
#' marginal_histogram_plot(iris, "Sepal.Width", "Petal.Length", "Sepal Length" , "Petal Length")
#'
marginal_histogram_plot <- function(data_in, xname, yname, margin_type = "histogram", xlab = NULL, ylab = NULL) {
  p <- ggplot(data_in, aes_string(xname,yname)) +
    geom_point() +
    theme_tufte() +
    theme(axis.title.x = element_text(vjust=-0.5),
          axis.title.y = element_text(vjust=1.5))
  ggMarginal(p, type = margin_type, fill="transparent")
}


#' Cumulative distribution plot
#'
#'
#' @param data_in Dataframe in
#' @param xlab Title of the X axis, default NULL
#' @param ylab Title of the Y axis, default NULL
#'
#' @import ggthemes ggplot2 ggExtra
#' @export cumulative_dist_plot
#'
#' @examples
#'
#' cumulative_dist_plot(iris$Sepal.Width)
#'
cumulative_dist_plot <- function(data_in, xlab = NULL, ylab = NULL) {
  quantiles <- data.frame(quantity = quantile(data_in, seq(0,100)/100), percentage=seq(0,100))
  p <- ggplot(quantiles, aes(quantity,percentage)) +
    geom_point(shape=3) +
    labs(x= xlab, y = ylab) +
    theme_bw()
}

#' Creates a barplot with error bars
#'
#' Straight from the R-book 2nd edition
#' 
#' @param yv the values of the bar chart
#' @param z the confidence levels
#' @param labels the labels
#'
#' @return
#' @export error.bars
#'
error.bars <- function(yv,z,labels){
  xv <- barplot(yv,ylim=c(0,(max(yv)+max(z))),names=labels,ylab=deparse(substitute(yv) ))
  g=(max(xv)-min(xv))/50
  for (i in 1:length(xv)) {
    lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i], yv[i]+z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i], yv[i]-z[i]))
  }
}