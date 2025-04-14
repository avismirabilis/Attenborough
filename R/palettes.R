#' Attenborough Color Palettes
#'
#' A collection of color palettes inspired by nature documentaries, narrated by Sir David Attenborough.
#'
#' @export
attenborough_palettes <- list(
  Polar = c("#75705C", "#855D58", "#A29EA2", "#8F9EBD", "#DFDFD0", "#7A9DA0", "#9F9288"),
  LifeOfBirds = c("#1E2A2B","#F0C202","#065747","#B61E00","#2399E7","#FFF163","#4B4600"),
  Leopards = c("#CEB68E","#7F7B72", "#B5BBB3", "#191715", "#5A4135", "#A27D4B", "#8E2F14"),
  Oceans = c("#00CAD3", "#4C667D", "#362220", "#E2E2CD", "#709B8C", "#77BADA", "#C1E2EF"),
  Tropical = c("#317002", "#A5A704", "#846C16", "#C4D294", "#182709", "#833002", "#99B691"),
  Tundra = c("#35273E", "#E2E8E8", "#3A2211", "#8A7E56", "#4E4667", "#635F37", "#7C7877"),
  Titans = c("#155E7A", "#071F29", "#C7E0E6", "#908F9C", "#2D9BBA", "#C28E6B", "#82522B")
)


#' Get an Attenborough color palette
#'
#' @param name The name of the palette
#' @param n Number of colors to return (optional)
#'
#' @export
attenborough_palette <- function(name, n = NULL) {
  pal <- attenborough_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found")

  if (is.null(n)) {
    n <- length(pal)
  }

  if (n > length(pal)) {
    warning("n is greater than the palette length. Returning the full palette.")
    return(pal)
  }

  return(pal[1:n])
}

#' Display an Attenborough color palette
#'
#' @param name The name of the palette
#'
#' @export
display_attenborough_palette <- function(name) {
  pal <- attenborough_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found")

  barplot(rep(1, length(pal)), col = pal, names.arg = pal, main = name)
}
