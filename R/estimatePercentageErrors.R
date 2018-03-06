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

estimatePercentageErrors <- function(x, conf.level = 0.95, coord.flip = FALSE, digits = 5) {
  if (!is.data.frame(x))
   stop("input must be a data.frame")
  
  percentage <- c()
  lo <- c()
  hi <- c()

  for (i in 1:length(x$cases)){
      percentage[i] <-  round(x$cases[i]/x$total[i], digits)
      lo[i] <- round(binom.test(x$cases[i], x$total[i], conf.level = conf.level)$conf.int[1], digits)
      hi[i] <- round(binom.test(x$cases[i], x$total[i], conf.level = conf.level)$conf.int[2], digits)
      }

  x$percentage <- percentage*100
  x$lo <- lo*100
  x$hi <- hi*100


  dodge <- ggplot2::position_dodge(width = 0.9)
  limits <- ggplot2::aes(ymax = x$hi,
                ymin = x$lo)


  p <- ggplot2::ggplot(x, ggplot2::aes(x=x$category, y=x$percentage, fill = x$category))
  p <- p + ggplot2::geom_bar(stat="identity")
  p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1))
  p <- p + ggplot2::geom_errorbar(limits, position = dodge, width = 0.25)
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  p <- p + ggplot2::ggtitle("Bar Plot of Percentage with errors")
  p <- p + ggplot2::labs(x="Category",y="Percentage")
  if( coord.flip == TRUE){
   p <- p + ggplot2::coord_flip()
  }

  print(p)
  return(x)

}
