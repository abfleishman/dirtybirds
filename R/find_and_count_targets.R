

#' find  and count possible targets in an image
#'
#'
#'
#' @param paths  a data frame with the full paths to the images in a column named "path".
#' @param keep1  the number of high-MH-distance 16x16 pixel squares to keep in the first pass.
#' @return A dataframe with counts of targets in each image
#' @importFrom magrittr %>%
#' @importFrom raster brick aggregate clump getValues raster setValues xyFromCell ncell
#' @importFrom stringr str_replace_all
#' @importFrom purrr map map2 pmap
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom stats mahalanobis cov
#' @import dplyr
#' @export


# img_dir<-'//NAS1/NAS3_2Mar15/Images/USGS_SoCal_Flights_2018_jpg_tiled/sampled'
# paths<-list.files(img_dir,full.names = T,recursive = T)
#
# paths<-data.frame(path=paths) %>% mutate(path=as.character(path))
find_and_count_targets <- function(paths, keep1 = 50) {

  raster_list_data_frame<-paths %>%
    mutate( name = str_replace_all(basename(paths$path) %>% gsub(".jpg","",.), "\\..*$", ""),
            x = map(path,brick),
            xagg = map(x, aggregate, fact = 16),
            xapca = map(xagg, rasterPCA),
            pc1 = map(xapca,function(xx)xx$map[[1]][]),
            pc2 = map(xapca,function(xx)xx$map[[2]][]),
            pc3 = map(xapca,function(xx)xx$map[[3]][])) %>%
    group_by(name) %>%
    mutate( df=list(tibble(pc1=unlist(pc1),pc2=unlist(pc2),pc3=unlist(pc3))),
            m_dist=map(df, function(yy) round(mahalanobis(yy, colMeans(yy), cov(yy)), 2)))

  # now imagine we want to keep the "keep" most outlier-ish (highest m-dist) points
  # keep the keep1 most outlier-ish
  cutoff<-raster_list_data_frame %>%
    dplyr::select(name,df,m_dist) %>%
    unnest(.) %>%
    group_by(name) %>%
    arrange(desc(m_dist)) %>%
    slice(keep1) %>%
    dplyr::select(name,cutoff=m_dist)

  #### Finding approximate centers ####

  raster_list_data_frame<-raster_list_data_frame %>%
    left_join(cutoff) %>%
    group_by(name) %>%
    mutate(outlier_maha = map(m_dist, function(zz) ifelse(zz >= cutoff, TRUE, FALSE)),
           tmp = map(xagg,function(zz) raster(zz)),
           tmp = map2(tmp,outlier_maha,function(xx,yy) setValues(xx,yy)),
           rc = map(tmp,clump),
           clump_id = map(rc,getValues),
           xy =  map(rc, function(xx) xyFromCell(xx,1:ncell(xx)))
    )

  counts<-raster_list_data_frame %>%
    dplyr::select(name,clump_id,m_dist) %>%
    unnest(.) %>%
    filter(!is.na(clump_id)) %>%
    group_by(name) %>%
    summarise(n=max(clump_id,na.rm=T))


  return(counts)

}
