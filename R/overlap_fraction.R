

#' Given hi and lo x coordinates of two squares, return the fraction of the second squares's area that overlaps the first
#'
#' The parameter here should be fairly obvious.
#' This is implemented vectorized on square 1.  So the square 1 parameters can
#' be vectors
#' @param xlo1 the low x value of square 1, etc...
#' @param use1 a logical vector.  If TRUE then the corresponding square one should
#' be considered and used to contribute to any overlap.
#' @keywords internal
overlap_fraction_single <- function(xlo1, xhi1, ylo1, yhi1, xlo2, xhi2, ylo2, yhi2, use1) {
  w <- 0  # default these to 0
  h <- 0
  A <- (xhi2 - xlo2) * (yhi2 - ylo2)  # total area of square 2

  w <- ifelse(xlo2 > xlo1 & xlo2 < xhi1, xhi1 - xlo2,
              ifelse(xhi2 > xlo1 & xhi2 < xhi1, xhi2 - xlo1, 0))

  h <- ifelse(ylo2 > ylo1 & ylo2 < yhi1, yhi1 - ylo2,
              ifelse(yhi2 > ylo1 & yhi2 < yhi1, yhi2 - ylo1, 0))

  ((w * h) / A) * use1
}



# pass this a data frame with xlo and xhi columns and this will return
# a tibble of two columns: cumul_overlap and keep
# @param max_cumul_overlap if cumul overlap exceeds this for a square, it gets tossed.
overlap_fraction <- function(D, max_cumul_overlap = 0.2) {
  ret <- tibble::tibble(cumul_overlap = rep(0, nrow(D)),
                keep = rep(TRUE, nrow(D)))

  for(i in 2:nrow(D)) {
    j <- i - 1
    ret$cumul_overlap[i] <- sum(overlap_fraction_single(D$xlo[1:j], D$xhi[1:j], D$ylo[1:j], D$yhi[1:j],
                            D$xlo[i], D$xhi[i], D$ylo[i], D$yhi[i],
                            ret$keep[1:j]))

    if(ret$cumul_overlap[i] > max_cumul_overlap) {
      ret$keep[i] <- FALSE
    }
  }
  ret
}




