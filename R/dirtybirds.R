# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if(getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      ".",
      "freq",
      "index",
      "is_clump",
      "keep",
      "max_mh",
      "mh_dist",
      "midx",
      "midy",
      "xhi",
      "xlo",
      "y",
      "yhi",
      "ylo"
     )
  )
}

