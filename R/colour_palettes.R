## Prince Discography ----

prince_palette <- list(
  ## 1999 ----
  Y1999 = c(
    "#FDB259", ## Yellow Orange
    "#8B4942", ## Brandy
    "#C52E45", ## French Raspberry
    "#468F88", ## Wintergreen Dream
    "#3D2D6B", ## Spanish Violet
    "#C0A1B0", ## Lilac Luster
    "#4A5B91"  ## Liberty
  ),
  ## Controversy ----
  Controversy = c(
    "#CAA589", ## Tumbleweed
    "#DE754F", ## Burnt Sienna
    "#813D2A", ## Burnt Umber
    "#541425", ## Dark Sienna
    "#9E6A6F", ## Copper Rose
    "#AD2960"  ## Maroon X 11
  ),
  ## PurpleRain ----
  PurpleRain = c(
    "#FEDE50", ## Mustard
    "#5F3F4D", ## Eggplant
    "#B76130", ## Alloy Orange
    "#442B83", ## Spanish Violet
    "#3370BC", ## Bright Navy Blue
    "#E4A0D4", ## Orchid Crayola
    "#819381", ## Xanadu
    "#BBDDF0"  ## Columbia Blue
  ),
  ## Diamonds and Pearls ----
  DiamondsPearls = c(
    "#303F92", ## Dark Cornflower Blue
    "#5774C8", ## Han Blue
    "#7EB7F2", ## French Sky Blue
    "#E68535", ## Cadmium Orange
    "#B55024", ## Rust
    "#611312"  ## Rosewood
  ),
  ## Parade ----
  Parade = c(
    "#161616", ## Eerie Black
    "#565656", ## Davys Grey
    "#828282", ## Gray Web
    "#E4E4E4", ## Platinum
    "#FFFFFF"  ## White
  ),
  ## Batman ----
  Batman = c(
    "#F7BE47", ## Maximum Yellow Red
    "#FCE9AF", ## Banana Mania
    "#E36A22", ## Spanish Orange
    "#AA3B1F", ## Chinese Red
    "#010101"  ## Black
  )
  ## Sign O the Times ----
  SignOtheTimes = c(
    "#842410", ## Kobe
    "#E4CE52", ## Arylide Yellow
    "#B36B9F", ## Pearly Purple
    "#D98D06", ## Harvest Gold
    "#D6C9EF", ## Thistle
    "#6E692A", ## Antique Bronze
    "#4E2B19"  ## Seal Brown
  )
)

#' @title Prince Discography palettes
#' @description 1999, Controversy, PurpleRain, DiamondsPearls, Parade,
#' Batman, SignOtheTimes, LoveSymbol, LoveSexy, AroundTheWorld, & Emancipation
#' @inheritDotParams ggplot2::discrete_scale
#' @param palette name of palette, Default: "PurpleRain"
#' @param n number of colours
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname discography_pal
#' @export
#' @examples
#' library(scales)
#' show_col(discography_pal(palette = "PurpleRain")(5))
#' show_col(discography_pal(palette = "DiamondsPearls")(5))
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

discography_pal <- function(palette = "PurpleRain", n,
                         type = c("discrete", "continuous"),
                         reverse = FALSE) {
  discography <- discography_palette[[palette]]

  if (reverse == TRUE) {
    discography <- rev(discography)
  }

  if (missing(n)) {
    n <- length(discography)
  }

  type <- match.arg(type)

  if (type == "discrete" && n > length(discography)) {
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(discography)}!"))
  }

  discography <- switch(type,
                     continuous = grDevices::colorRampPalette(discography)(n),
                     discrete = discography[1:n])

  discography <- scales::manual_pal(discography)

  return(discography)
}

#' @title scale_color_westeros
#' @rdname discography_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), colour = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_discography(palette = "PurpleRain")
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), colour = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_discography(palette = "PurpleRain")
#' @importFrom ggplot2 discrete_scale scale_colour_gradientn

scale_color_discography <- function(palette = "PurpleRain", n, type = "discrete",
                                 reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("colour", "discography",
                            discography_pal(palette = palette, n = n, type = type,
                                         reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_color_gradientn(colors = discography_pal(palette = palette, n = n, type = type,
                                                         reverse = reverse)(8))
  }
}

#' @title scale_colour_discography
#' @rdname discography_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_discography(palette = "PurpleRain")
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_discography <- scale_color_westeros

#' @title scale_fill_discography
#' @rdname discography_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'      geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#'      scale_fill_discography(palette = "PurpleRain")
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_discography <- function(palette = "PurpleRain", n, type = "discrete",
                                reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "discography",
                            discography_pal(palette = palette, n = n, type = type,
                                         reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_fill_gradientn(colors = discography_pal(palette = palette, n = n, type = type,
                                                        reverse = reverse)(8))
  }
}


# --- End of Script ---
