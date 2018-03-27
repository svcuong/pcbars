#' Estimate percentage errors
#'
#' Functions to estimate percentage errors
#'
#'
#' @aliases estimatePercentageErrors
#'
#' @param x: A data frame containing colums category, cases and total as input.
#' @param conf.level: Confidence level for the returned confidence interval, default 0.95.
#' @param coord.flip: Cartesian coordinates with x and y flipped, default FALSE.
#' @param digits: Integer indicating the number of decimal places to be used, default 5.
#' @param sort: A logical value. If TRUE, the result are sorted by percentage .
#' @param plotly: A logical value. If TRUE, the grpaph are plotted as plotly object.
#' @param title: A title for the graph, default "Bar Plot of Percentage with errors".
#' @param xlab: A title for the x axis, default "Category".
#' @param ylab: A title for the y axis, default "Percentage".
#' @return \code{estimatePercentageErrors} returns a data frame as the input with columns percentage,
#' lo and hi and a bar plot of percentage with errors.
#' @author Andrey Davydenko, Maxim and Sai Van Cuong from Volgograd State Technical University
#' @examples
#'
#' estimatePercentageErrors(data20)
#' estimatePercentageErrors(data20, conf.level = 0.8)
#' estimatePercentageErrors(mydata, conf.level = 0.8, coord.flip = TRUE)
#' estimatePercentageErrors(mydata, conf.level = 0.8, coord.flip = TRUE, digits = 2)
#'
#' @export
#' @export estimatePercentageErrors

estimatePercentageErrors <- function(x, conf.level = 0.95, coord.flip = FALSE, digits = 5, sort = FALSE,
                                     plotly = FALSE, title = "Bar Plot of Percentage with errors",
                                     xlab = "Category", ylab = "Percentage") {
# Check class of input
  if (!is.data.frame(x))
   stop("input must be a data.frame")

# Create a empty vectors
  percentage <- c()
  lo <- c()
  hi <- c()
# calculate percentage with confidence interval
  for (i in 1:length(x$cases)){
      percentage[i] <-  round(x$cases[i]/x$total[i], digits)
      lo[i] <- round(binom.test(x$cases[i], x$total[i], conf.level = conf.level)$conf.int[1], digits)
      hi[i] <- round(binom.test(x$cases[i], x$total[i], conf.level = conf.level)$conf.int[2], digits)
      }
# add percentage and lo, hi columns into data frame as input
  x$percentage <- percentage*100
  x$Lo <- lo*100
  x$Hi <- hi*100
# sorting x by percentage for plotting
 if (sort == TRUE) {
  x$category <-  factor(x$category, levels = unique(x$category)[order(x$percentage)])
 }
# Changing column names of Hi and Lo
  colnames(x)[5] <- paste("Lo", conf.level*100)
  colnames(x)[6] <- paste("Hi", conf.level*100)

# Create a bar plot with difference options
  dodge <- ggplot2::position_dodge(width = 0.9)
  limits <- ggplot2::aes(ymax =  x[,6],
                ymin = x[, 5])

  p <- ggplot2::ggplot(x, ggplot2::aes(x=x$category, y=x$percentage, fill = x$category))
  p <- p + ggplot2::geom_bar(stat="identity")
  p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1))
  p <- p + ggplot2::geom_errorbar(limits, position = dodge, width = 0.25)
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  p <- p + ggplot2::ggtitle(title)
  p <- p + ggplot2::labs(x= xlab, y= ylab)
  if( coord.flip == TRUE){
   p <- p + ggplot2::coord_flip()
  }

  if (plotly == TRUE){
    p <- plotly::ggplotly(p)
  }

  if (sort == TRUE){
    x <- x[order(x$percentage),]
  }

  print(p)
  return(x)
}
