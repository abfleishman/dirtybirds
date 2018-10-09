

#' find possible targets and crop out little squares around them
#'
#' Plots a 100 x 100 sheet of possible targets and a figure showing where
#' they occur.
#' @param img the path to the image.
#' @param keep1  the number of high-MH-distance 16x16 pixel squares to keep in the first pass.
#' @param keep2  the number of squares that actually gets kept to be reported.
#' The actual number may be less than this because overlapping squares get tossed.
#' @param outdir the directory in which to write the results
#' @param crop_pixels how many pixels on a side in what gets cropped out?
#' @param max_cumul_overlap if a square overlaps in area cumulatively by more than this amount with any other squares with
#' better targets, it is tossed
#' @export
find_and_crop_targets <- function(img, keep1 = 200, keep2 = 100, outdir = ".", crop_pixels = 150, max_cumul_overlap = 0.2) {
  name <- basename(img) %>%
    stringr::str_replace_all("\\..*$", "")

  x <- raster::brick(img)

  xagg <- raster::aggregate(x, fact = 16)

  xapca <- rasterPCA(xagg)

  pc1 <- xapca$map[[1]][]
  pc2 <- xapca$map[[2]][]
  pc3 <- xapca$map[[3]][]

  # get the name of an output directory where all of this will be
  # written.  And create it if it is not there
  write_dir <- file.path(outdir, name)
  if(!dir.exists(write_dir)) {
    dir.create(write_dir, recursive = TRUE)
  }



  #### Mahalanobis distance calcs ####
  df <- tibble::tibble(pc1 = pc1, pc2 = pc2, pc3 = pc3)
  m_dist <- stats::mahalanobis(df[, 1:3], colMeans(df[, 1:3]), stats::cov(df[, 1:3]))
  df$m_dist <- round(m_dist, 2)

  # now imagine we want to keep the "keep" most outlier-ish (highest m-dist) points
  # keep the keep1 most outlier-ish
  cutoff <- df %>%
    dplyr::arrange(dplyr::desc(m_dist)) %>%
    dplyr::slice(keep1) %>%
    .$m_dist


  # Mahalanobis Outliers - Threshold set to cutoff
  df <- df %>%
    dplyr::mutate(outlier_maha = ifelse(m_dist >= cutoff, TRUE, FALSE))



  #### Finding approximate centers ####
  tmp <- raster::raster(xagg)
  tmp[df$outlier_maha] <- 1
  tmp[!df$outlier_maha] <- 0

  rc <- raster::clump(tmp)
  clump_id <- raster::getValues(rc)
  xy <- raster::xyFromCell(rc,1:raster::ncell(rc))
  DF <- data.frame(xy, clump_id, is_clump = rc[] %in% raster::freq(rc, useNA = 'no')[,1]) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(mh_dist = df$m_dist)

  # now let's get the X-Y centers of each of those clumps by simply averaging them
  # and let us keep track of the maximal mahalanobis distance amongst the clump.
  clump_centers <- DF %>%
    dplyr::filter(is_clump) %>%
    dplyr::group_by(clump_id) %>%
    dplyr::summarise(
      num = dplyr::n(),
      midx = as.integer(mean(x)),
      midy = as.integer(mean(y)),
      max_mh = max(mh_dist)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(max_mh))

  # while we are at it, let's make
  # squares around these
  squares <- clump_centers %>%
    dplyr::mutate(xlo = as.integer(midx - crop_pixels / 2),
           xhi = as.integer(midx + crop_pixels / 2),
           ylo = as.integer(midy - crop_pixels / 2),
           yhi = as.integer(midy + crop_pixels / 2)) %>%
    dplyr::mutate(index = 1:dplyr::n())

  # now keep just the top keep2 of those
  keep2_top_squares <- squares %>%
    dplyr::filter(index <= keep2)

  # now plot their positions in the image:
  pospicO <- ggplot2::ggplot(keep2_top_squares) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xlo, xmax = xhi, ymin = ylo, ymax = yhi), fill = NA, color = "red") +
    ggplot2::geom_text(ggplot2::aes(x = midx, y = midy, label = index))


  ggplot2::ggsave(pospicO, filename = file.path(write_dir, paste0("positions-before-overlap-removal-", name, ".pdf")), width = 12, height = 12)


  # now toss out those squares that overlap too much with the more outlierish targets
  keep2_top_squares_olaps <- dplyr::bind_cols(keep2_top_squares, overlap_fraction(keep2_top_squares, max_cumul_overlap))

  top_squares <- keep2_top_squares_olaps %>%
    dplyr::filter(keep == TRUE) %>%
    dplyr::mutate(index = 1:dplyr::n())


  # now plot their positions in the image:
  pospic <- ggplot2::ggplot(top_squares) +
    ggplot2::geom_rect(ggplot2::aes(xmin = xlo, xmax = xhi, ymin = ylo, ymax = yhi), fill = NA, color = "red") +
    ggplot2::geom_text(ggplot2::aes(x = midx, y = midy, label = index))


  ggplot2::ggsave(pospic, filename = file.path(write_dir, paste0("positions-", name, ".pdf")), width = 12, height = 12)


  #### Now, get ready to crop those bits out  and then plot them ####
  extents_to_grab <- lapply(1:nrow(top_squares), function(i) {
    r <- top_squares[i, ]
    raster::extent(
      max(0, r$xlo),
      min(raster::extent(x)[2], r$xhi),
      max(0, r$ylo),
      min(raster::extent(x)[4], r$yhi)
    )
  })




  # don't bother mclapplying this, because we are likely going to job array
  # this stuff over the different images
  interesting_squares <- lapply(extents_to_grab, function(E) raster::crop(x, E))


  # write out the "proof sheet"
  grDevices::pdf(file = file.path(write_dir, paste0("proof-sheet-", name, ".pdf")), width = 44, height = 44)
  graphics::par(mfrow=c(10,10))
  dump <- lapply(seq_along(interesting_squares), function(i) {
    s <- interesting_squares[[i]]
    raster::plotRGB(s)
    graphics::text(x = raster::xmax(s) - 0.05 * crop_pixels,
         y = raster::ymax(s) - 0.05 * crop_pixels,
         label = i,
         adj = c(1, 1),
         col = "white",
         cex = 3.0
         )
  })
  grDevices::dev.off()

  # and then write out the individual tiles in the write_dir,
  # and make sure that the extent coordinates are simply in the names
  # of the files.
  dump <- lapply(seq_along(interesting_squares), function(i) {
    s <- interesting_squares[[i]]
    grDevices::pdf(file = file.path(write_dir, paste0("tile_", name, "_",
                                           sprintf("%03d", i), "-",
                                           raster::xmin(s), "-",
                                           raster::xmax(s), "-",
                                           raster::ymin(s), "-",
                                           raster::ymax(s),
                                           ".pdf")))
    raster::plotRGB(s)
    grDevices::dev.off()

  })


  return(name)

}
