#' cividis palette (discrete)
#'
#' @details
#'
#' See \code{cividis} for more information on the color scale.
#'
#' @param alpha pass through parameter to \code{cividis}
#'
#' @param begin The (corrected) hue in [0,1] at which the cividis colormap begins.
#'
#' @param end The (corrected) hue in [0,1] at which the cividis colormap ends.
#'
#' @param direction Sets the order of colors in the scale. If 1, the default, colors
#' are ordered from darkest to lightest. If -1, the order of colors is reversed.
#'
#' @param option A character string indicating the colormap option to use. So far, there is only one
#' option called "cividis" (or "V").
#'
#' @examples
#' library(scales)
#' show_col(cividis_pal()(10))
#'
#'
#' @export
cividis_pal <- function(alpha = 1, begin = 0, end = 1, direction = 1, option= "V") {
  function(n) {
    cividis(n, alpha, begin, end, direction, option)
  }
}


#' @rdname scale_cividis
#'
#' @importFrom ggplot2 scale_fill_gradientn scale_color_gradientn discrete_scale
#'
#' @export
scale_color_cividis <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                                discrete = FALSE, option = "V") {
  if (discrete) {
    discrete_scale("colour", "cividis", cividis_pal(alpha, begin, end, direction, option), ...)
  } else {
    scale_color_gradientn(colours = cividis(256, alpha, begin, end, direction, option), ...)
  }
}

#' @rdname scale_cividis
#' @aliases scale_color_cividis
#' @export
scale_colour_cividis <- scale_color_cividis

#' cividis color scales
#'
#' Uses the cividis color scale.
#'
#' For \code{discrete == FALSE} (the default) all other arguments are as to
#' \link[ggplot2]{scale_fill_gradientn} or \link[ggplot2]{scale_color_gradientn}.
#' Otherwise the function will return a \code{discrete_scale} with the plot-computed
#' number of colors.
#'
#' See \code{cividis} for more information on the color scale.
#'
#' @param ... parameters to \code{discrete_scale} or \code{scale_fill_gradientn}
#'
#' @param alpha pass through parameter to \code{cividis}
#'
#' @param begin The (corrected) hue in [0,1] at which the cividis colormap begins.
#'
#' @param end The (corrected) hue in [0,1] at which the cividis colormap ends.
#'
#' @param direction Sets the order of colors in the scale. If 1, the default, colors
#' are as output by \link[cividis]{cividis_pal}. If -1, the order of colors is reversed.
#'
#' @param discrete generate a discrete palette? (default: \code{FALSE} - generate continuous palette)
#'
#' @param option A character string indicating the colormap option to use. At the moment, only option "cividis" (or "V") is available.
#'
#' @rdname scale_cividis
#'
#' @author Marco Sciaini \email{msciain@uni-goettingen.de} / \href{https://twitter.com/msciain}{@@msciain},
#'         C?dric Scherer \email{scherer@izw-berlin.de} / \href{https://twitter.com/CedScherer}{@@CedScherer}
#'
#' @importFrom ggplot2 scale_fill_gradientn scale_color_gradientn discrete_scale
#'
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' library(ggplot2)
#'
#' # ripped from the pages of ggplot2
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_point(size=4, aes(colour = factor(cyl))) +
#'     scale_color_cividis(discrete=TRUE) +
#'     theme_bw()
#'
#' # ripped from the pages of ggplot2
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
#' d <- ggplot(dsub, aes(x, y, colour=diff)) + geom_point()
#' d + scale_color_cividis() + theme_bw()
#'
#'
#' # ripped from the main viridis example
#' dat <- data.frame(x = rnorm(10000), y = rnorm(10000))
#'
#' ggplot(dat, aes(x = x, y = y)) +
#'   geom_hex() + coord_fixed() +
#'   scale_fill_cividis() + theme_bw()
#'
#' library(ggplot2)
#' library(MASS)
#' library(gridExtra)
#'
#' data("geyser", package="MASS")
#'
#' ggplot(geyser, aes(x = duration, y = waiting)) +
#'   xlim(0.5, 6) + ylim(40, 110) +
#'   stat_density2d(aes(fill = ..level..), geom="polygon") +
#'   theme_bw() +
#'   theme(panel.grid=element_blank()) -> gg
#'
#' grid.arrange(
#'   gg + scale_fill_cividis(option="V") + labs(x="Virdis V", y=NULL),
#'   ncol=2, nrow=2
#' )
#'
#' @export
scale_fill_cividis <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                               discrete = FALSE, option = "V") {
  if (discrete) {
    discrete_scale("fill", "cividis", cividis_pal(alpha, begin, end, direction, option), ...)
  } else {
    scale_fill_gradientn(colours = cividis(256, alpha, begin, end, direction, option), ...)
  }
}
