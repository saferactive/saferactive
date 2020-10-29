

#' Break a line into segments
#'
#' @param l line
#' @param n_segments number of segments
#' @param segment_length segment length
#' @export
#' @family OSM
#' @return list
#' @details see stplanr::line_segment note: does not make perfect breaks
#'
line_segment_sf <- function(l, n_segments, segment_length = NA) {
  if (!is.na(segment_length)) {
    l_length <- as.numeric(sf::st_length(l))
    l$n_segments <- ceiling(l_length / segment_length)
  }

  l_nosplit <- l[l$n_segments < 2, ]
  l_split <- l[l$n_segments >= 2, ]

  l_split$split_id <- seq(1,nrow(l_split))
  l_split <- dplyr::group_by(l_split, split_id)
  l_split <- dplyr::group_split(l_split)

  #attrib <- sf::st_drop_geometry(l_split)
  #geom <- sf::st_geometry(l_split)

  #attrib <- attrib[rep(seq(1,nrow(attrib)), times = l_split$n_segments),]

  split_int <- function(x){
    ln <- sf::st_geometry(x)
    att <- sf::st_drop_geometry(x)
    n <- att$n_segments
    att <- att[rep(1, n),]

    # ln <- geom[i]
    # n <- l_split$n_segments[i]
    # if(n < 2){
    #   return(ln)
    # }
    pts <- sf::st_cast(sf::st_sfc(ln),"POINT")
    pts <- pts[!duplicated(pts)]
    lth <- length(pts)
    if(lth < (n + 1)){
      # Not enough points to do a simple split
      ln <- sf::st_segmentize(ln, ceiling(as.numeric(sf::st_length(ln)) / n ))
      pts <- sf::st_cast(sf::st_sfc(ln),"POINT")
      pts <- pts[!duplicated(pts)]
      lth <- length(pts)
    }

    brks <- seq_len(lth)[!duplicated(ceiling(seq_len(lth)/(lth/n)))]
    brks <- brks[seq(2,length(brks))]

    # edge case when lth ~ n
    if(length(brks) != (n - 1)){
      brks <- brks[seq(1,n-1)]
    }

    #pts <- pts[brks]
    #res <- lwgeom::st_split(ln, pts)
    # Break and rebuild
    ln <- sf::st_coordinates(ln)
    brks2 <- c(seq(1,nrow(ln)), brks)
    brks2 <- brks2[order(brks2)]
    ln <- ln[brks2,]
    brks3 <- seq(1,nrow(ln))[duplicated(brks2)]
    brks3 <- brks3 - 1

    split_pattern <- function(val = 6, bk = c(2,5)){
      if(length(bk) == 1){
        ints = bk
      } else {
        ints = c(0,bk[seq(1,length(bk) - 1)])
        ints = bk - ints
        ints = ints
      }
      res = rep(seq(1, length(bk)), times = ints)
      res = c(res, rep(max(res) + 1, times = val - length(res)))
      return(res)
    }

    f = split_pattern(nrow(ln), brks3)
    f2 = c(seq_len(nrow(ln)),brk)

    ln2 <- split(ln[,1:2], f)
    ln2 <- lapply(ln2, function(y){matrix(y, ncol = 2)})

    res <- sf::st_multilinestring(ln2)
    res <- sf::st_sfc(res)

    #plot(x$geometry, lwd = 4)
    #plot(res, add = T, lwd = 2, col = "red")
    res <- sf::st_cast(res, "LINESTRING")
    sf::st_geometry(att) <- res
    sf::st_crs(att) <- sf::st_crs(x)
    return(att)
  }

  if(ncores == 1){
    res_split <- pbapply::pblapply(l_split, FUN = split_int)

  } else {
    cl <- parallel::makeCluster(ncores)
    parallel::clusterExport(
      cl = cl,
      varlist = c("attrib"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      library(sf)
      # library(dplyr)
    })
    res_split <- pbapply::pblapply(l_split,
                                   FUN = split_int,
                                   cl = cl)

  }

 res_split <- dplyr::bind_rows(res_split)



  res_final <- rbind(res_split, l_nosplit)
  return(res_final)

  # geom <- pbapply::pblapply(seq(1, length(geom)),
  #                          FUN = split_int)
  # geom <- unlist(geom, recursive = FALSE)
  # geom <- st_as_sfc(geom)
  #
  # st_geometry(attrib) <- geom
  # st_crs(attrib) <- st_crs(l)



  # return(attrib)
}

#' Split a linestrings into straight segments
#'
#' @param sl A spatial object representing routes on a transport network
#' @param quiet Should the the function omit messages? `NULL` by default,
#' which means the output will only be shown if `sl` has more than 1000 rows.
#' @family rnet
#' @author Malcolm Morgan
#' @export
#' @return An `sf` object representing a route network
#' @rdname overline
fracture_line <- function(sl , quiet = NULL) {
  if (!"sfc_LINESTRING" %in% class(sf::st_geometry(sl))) {
    stop("Only LINESTRING is supported")
  }
  if (is.null(quiet)) {
    quiet <- ifelse(nrow(sl) < 1000, TRUE, FALSE)
  }
  sl <- sf::st_zm(sl)
  sl_crs <- sf::st_crs(sl)
  if (!quiet) {
    message(paste0(Sys.time(), " constructing segments"))
  }
  c1 <- sf::st_coordinates(sl)
  sf::st_geometry(sl) <- NULL
  l1 <- c1[, 3] # Get which line each point is part of
  c1 <- c1[, 1:2]
  l1_start <- duplicated(l1) # find the break points between lines
  l1_start <- c(l1_start[2:length(l1)], FALSE)
  c2 <- c1[2:nrow(c1), 1:2] # Create new coords offset by one row
  c2 <- rbind(c2, c(NA, NA))
  c2[nrow(c1), ] <- c(NA, NA)
  c2[!l1_start, 1] <- NA
  c2[!l1_start, 2] <- NA
  c3 <- cbind(c1, c2) # make new matrix of start and end coords
  rm(c1, c2)
  c3 <- c3[!is.na(c3[, 3]), ]
  sl <- sl[l1[l1_start], , drop = FALSE] # repeate attributes
  rm(l1, l1_start)

  # Make Geometry
  if (!quiet) {
    message(paste0(Sys.time(), " building geometry"))
  }
  sf::st_geometry(sl) <- sf::st_as_sfc(
    if (requireNamespace("pbapply", quietly = TRUE)) {
      pbapply::pblapply(1:nrow(c3), function(y) {
        sf::st_linestring(matrix(c3[y, ], ncol = 2, byrow = T))
      })
    } else {
      lapply(1:nrow(c3), function(y) {
        sf::st_linestring(matrix(c3[y, ], ncol = 2, byrow = T))
      })
    },
    crs = sl_crs
  )
  return(sl)

}
