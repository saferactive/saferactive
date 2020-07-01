#' collate area specific 1979 to 2018/19 all accidents by lat long
#' This is would be much better in a database like Mongo or PostGIS
#'
#' Experimenting with reading area specific entire data and visualizin
#' in geoplumber using eAtlas
#'
#' @param area one of the areas in local_authority_distict from STATS19
#'
#'
get_stats19(area = "London",
            dd = "~/code/saferactive/ignored/") {
  library('stats19')
  if(!exists("area")) {
    stop("Area is required")
  }
  # read in 1979 to 04
  acc7904 = get_stats19(year = 1979, data_dir = dd)
  # read in 05 to 14
  acc0514 = get_stats19(year = 2005, data_dir = dd)
  # read in 15 to 18
  acc1518 = get_stats19(year = 2015:2018, data_dir = dd)
  # identical(names(acc7904), names(acc0514))
  # identical(names(acc7904), names(acc1518))
  # n = sample(length(acc0514), 1)
  # names(acc7904)[n] == names(acc1518)[n]
  acc7918 = rbind(acc7904, acc0514)
  acc7918 = rbind(acc7918, acc1518)
  area7918 = acc7918[acc7918$local_authority_district == area,]
  return(area7918)
  names(acc7918)
  # saveRDS(acc7918, "acc7918.Rds")
}
