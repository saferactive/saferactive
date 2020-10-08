library(stats19)
year = 2018
n = c("caNotINac","veNotINac","acNotINca", "acNotINve")
check = function(year = 2019) {
  if(is.null(year) || is.na(year)) stop("Year is required")
  ac = get_stats19(year = year, type = "ac", output_format = "sf")
  ca = get_stats19(year = year, type = "ca")
  ve = get_stats19(year = year, type = "ve")

  # all(ca$accident_index %in% ac$accident_index)
  # all(ve$accident_index %in% ac$accident_index)

  caNotINac = which(!ca$accident_index %in% ac$accident_index)
  veNotINac = which(!ve$accident_index %in% ac$accident_index)
  acNotINca = which(!ac$accident_index %in% ca$accident_index)
  acNotINve = which(!ac$accident_index %in% ve$accident_index)
  v = list(caNotINac, veNotINac, acNotINca, acNotINve)
  names(v) = n
  return(v)
}
checks = lapply(2015:2019, function(x) check(x))
l = lapply(checks, function(x) list(
  caNotINac = length(x[[1]]),
  veNotINac = length(x[[2]]),
  acNotINca = length(x[[3]]),
  acNotINve = length(x[[4]])
))
d = as.data.frame(matrix(unlist(l), nrow=length(unlist(l[1]))))
names(d) = 2015:2019
rownames(d) = n
d
