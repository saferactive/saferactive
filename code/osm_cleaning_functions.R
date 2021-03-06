#' Select the main roads from OSM
#'
#' @param x A data frame of OSM lines
#' @export
#' @family OSM
#' @return Returns an data frame
#'
#' @details The OpenSteetMap contains a lot of detail, this function subsets the
#'   data to just the main roads used by cars by filtering on the highway tag.
#' @examples
#' \dontrun{
#' osm <- osmextract::oe_read("greater-london-latest.osm.pbf")
#' osm <- osm_main_roads(osm)
#' }
#'
osm_main_roads <- function(x){
  x <- x[!is.na(x$highway),]
  x <- x[x$highway %in% c("primary","primary_link",
                          "secondary","secondary_link",
                          "tertiary","tertiary_link",
                          "trunk","trunk_link",
                          "motorway","motorway_link",
                          "unclassified","residential",
                          "road","mini_roundabout"),]
  x
}


#' Consolidate roads into simplified network
#'
#' @param x a SF data frame of OSM linestrings with projected CRS
#' @param segment numeric, max length of segments in metres
#' @export
#' @family OSM
#' @return Returns an data frame of LINESTRINGS
#'
#' @details This function simplifies a OSM road network by: 1) grouping small
#'   road segments into single names roads 2) splitting long roads into sections
#'   are defined by `segment` 3) Casting MULTILINESTRINGs into LINESTRING
#'
#'   Note: to avoid splitting short roads, roads are only split once they are 2x
#'   `segment`, but are then splits into lengths as defined by `segment`. For
#'   example a 600m road will not be split, but a 1100m road will be split into
#'   approximately 500m segments.
#' @examples
#' \dontrun{
#' osm <- osmextract::oe_read("greater-london-latest.osm.pbf")
#' osm <- osm_main_roads(osm)
#' osm <- sf::st_transform(osm, 27700)
#' osm <- osm_consolidate(osm)
#' }
#'
osm_consolidate <- function(x, segment = 500){
  if(sf::st_is_longlat(x)){
    stop("Must use projected coordinates")
  }
  x <- x[,c("name", "ref", "highway")]
  # Group
  x <- dplyr::group_by(x, name, ref, highway)
  x <- dplyr::summarise(x, do_union = FALSE)
  # merge MULITLINESTRING into LINESTRING
  xls <- x[sf::st_geometry_type(x) == "LINESTRING",]
  xmls <- x[sf::st_geometry_type(x) == "MULTILINESTRING",]
  xmls <- sf::st_line_merge(xmls)
  xmlsA <- xmls[sf::st_geometry_type(xmls) == "LINESTRING",]
  xmlsB <- xmls[sf::st_geometry_type(xmls) == "MULTILINESTRING",]
  xmlsB <- sf::st_cast(xmlsB, "LINESTRING")
  x_merge <- list(xls, xmlsA, xmlsB)
  x_merge <- dplyr::bind_rows(x_merge)

  x_merge$length <- as.numeric(sf::st_length(x_merge))
  x_lth <- x_merge$length > (2 * segment)
  x_long <- x_merge[x_lth, ]
  x_short <- x_merge[!x_lth, ]

  x_long <- line_segment_sf(x_long, segment_length = segment)
  res <- rbind(x_short,x_long)
  res$length <- NULL
  res$length <- as.numeric(sf::st_length(res))
  return(res)
}

#' Extract junction points from OSM road linestrings
#'
#' @param x a SF data frame of OSM linestrings
#' @export
#' @family OSM
#' @return Returns an SF data frame of POINTS
#'
#' @details This function finds all junction points in a road network, i.e.
#'   where two roads meet. It excludes road crossings e.g. bridges.
#' @examples
#' \dontrun{
#' junctions <- osm_get_junctions(osm)
#' }
#'
osm_get_junctions <- function(x){
  points <- sf::st_cast(x,"MULTIPOINT")
  points <- points$geometry
  points <- sf::st_cast(points,"POINT")
  # TO be a junction their must be duplication of points
  dup <- duplicated(points)
  points <- points[dup]
  # But we only want on version of the junction
  dup <- duplicated(points)
  points <- points[!dup]
  return(points)
}

#' Cluster junction points into polygons
#'
#' @param x a SF data frame of joints
#' @param dist buffer distance past to sf::st_buffer
#' @export
#' @family OSM
#' @return Returns an SF data frame of POLYGONS
#'
#' @details This function clusters points together and defines junction
#'   polygons, the size of the polygons is dictated by `dist`. For single
#'   junction points a circle around the junction point is returned. For
#'   clustered junctions a polygon enclosing the whole junction area is
#'   returned. A column called junction_ids provides a looup list between the
#'   junction clusters and the junction points.
#' @examples
#' \dontrun{
#' junctions <- osm_get_junctions(osm)
#' }
#'
cluster_junction <- function(x, dist = 15){
  buff <- sf::st_buffer(x, dist = dist, nQuadSegs = 15)
  ints <- sf::st_intersects(buff)
  message(paste0("Clustering ",length(ints)," junctions"))
  ints_clus <- cluster_ints(ints)
  ints_clus <- ints_clus[lengths(ints_clus) > 0]

  message(paste0("Creating ",length(ints_clus)," geometries"))
  geoms <- list()
  pb <- progress_bar$new(total = length(ints_clus))
  for(i in seq_len(length(ints_clus))){
    pb$tick()
    sub <- buff[ints_clus[[i]]]
    if(length(sub) == 1){
      geoms[[i]] <- sub[[1]]
    } else {
      geoms[[i]] <- sf::st_union(sub)[[1]]
    }

  }
  attributes(geoms) <- attributes(buff)
  res <- data.frame(cluster_id = seq(1, length(geoms)))
  res$junction_ids <- ints_clus
  res$geometry <- geoms
  res <- sf::st_sf(res, crs = st_crs(x))
  return(res)
}

#' Internal Function
#'
#' @param x list of intersections from sf::st_intersects
#' @noRd
#' @family internal
#' @return Returns integer
#' @details Identifies clusters of intersecting geometries objects
#'
cluster_ints <- function(x){
  res <- list()
  pb <- progress_bar$new(total = length(x))
  for(i in seq_len(length(x))){
    pb$tick()
    if(length(x[[i]]) == 1){
      if(!is.na(x[[i]])){
        # Single Junction
        res[[i]] <- x[[i]]
      }
      # Else move on
    } else {
      # Multi-Junction Cluster
      sub <- recursive_ints(x[[i]], x = x)
      res[[i]] <- sub
      x[sub] <- NA
    }
  }
  return(res)
}

#' Internal Function
#'
#' @param sub integer
#' @param x list of integer
#' @noRd
#' @family internal
#' @return integer
#' @details recursively searches a list for values
#'
recursive_ints <- function(sub, x){
  sub <- unique(sub)
  sub <- sub[order(sub)]

  sub2 <- unique(unlist(x[sub]))
  sub2 <- sub2[order(sub2)]

  if(identical(sub, sub2)){
    return(sub)
  } else {
    return(recursive_ints(sub2, x = x))
  }

}

#' Fast search for the nearest point to another set of points
#'
#' @param x a SF data frame of POINTS
#' @param y a SF data frame of POINTS
#' @param clusters a list of integers, default NULL
#' @export
#' @family OSM
#' @return Returns an list of ids and distances
#'
#' @details If `clusters` is null will find the nearest y point for each x point
#'   and return a list of two vectors indexes of y length(x) and a distances
#'   length(x). If `clusters` is a list e.g. from `cluster_junction` then the
#'   indexes are replaced with the matching indexes of `clusters`
#' @examples
#' \dontrun{
#' junctions <- osm_get_junctions(osm)
#' junction_clusters <- cluster_junction(junctions)
#'
#' # Find the nearest junction
#' near_junction <- nn_point(crash_junction, junctions)
#'
#' # Find the nearest junction cluster
#' near_cluster <- nn_point(crash_junction, junctions, clusters = junction_clusters$junction_ids)
#' }
#'
nn_point <- function(x, y, clusters = NULL){
  if(sf::st_is_longlat(x)){
    stop("x must use projected coordinates")
  }
  if(sf::st_is_longlat(y)){
    stop("y must use projected coordinates")
  }
  if(!"sfc_POINT" %in% class(sf::st_geometry(x))){
    stop("x must be POINT")
  }
  if(!"sfc_POINT" %in% class(sf::st_geometry(y))){
    stop("y must be POINT")
  }
  x <- sf::st_coordinates(x)
  y <- sf::st_coordinates(y)

  nn <- nabor::knn(y, x, k = 1, eps = 0, searchtype = 1L, radius = 0)
  dist <- as.numeric(nn$nn.dists)
  indx <- as.integer(nn$nn.idx)

  if(is.null(clusters)){
    result <- list(point_index = indx,
                   distance = dist)
    return(result)
  }

  lookup <- rep(seq_along(clusters), vapply(clusters, length, FUN.VALUE = 1L))
  indx_clus <- lookup[match(indx, unlist(clusters))]

  warning("Returned distacnes are to points not clusters")
  result <- list(cluster_index = indx_clus,
                 distance = dist)
  return(result)


}

#' Fast search for the nearest lines to a set of points
#'
#' @param point a SF data frame of POINTS with projected CRS
#' @param lines a SF data frame of lines with projected CRS
#' @param k integer how many lines to search
#' @param ncores integer how many cores to use in parallel processing, default =
#'   1
#' @export
#' @family OSM
#' @return Returns an list of ids and distances
#'
#' @details The nearest line to a point is a non-trivial calculation which in
#'   theory requires checking the distance to every possible line. This function
#'   takes a shortcut by measuring distances to the centroids of lines, and then
#'   cross checking only nearby lines. The number of lines checked is defined by
#'   `k`. This process cannot guarantee 100% accuracy by is many orders of
#'   magnitude faster. To increase accuracy increase `k`.
#' @examples
#' \dontrun{
#' crash_road_nn <- nn_line(crash_road, osm, ncores = 5)
#' }
#'
nn_line <- function(point, lines, k = 50, ncores = 1){
  if(sf::st_is_longlat(point)){
    stop("point must use projected coordinates")
  }
  if(sf::st_is_longlat(lines)){
    stop("lines must use projected coordinates")
  }
  if(!"sfc_POINT" %in% class(sf::st_geometry(point))){
    stop("point must be POINT")
  }
  lines <- sf::st_geometry(lines)
  point <- sf::st_geometry(point)
  cents <- sf::st_centroid(lines)
  cents <- sf::st_coordinates(cents)
  message(paste0(Sys.time()," finding approximate distance for nearest ",k," centroids"))
  nn <- nabor::knn(cents, sf::st_coordinates(point), k = 50, eps = 0, searchtype = 1L, radius = 0)
  nn <- nn$nn.idx
  nn <- split(nn, 1:nrow(nn))

  message(paste0(Sys.time()," preparing inputs"))
  input <- purrr::pmap(.l = list(nn, point), z = lines, crs = sf::st_crs(point), .f = function(x,y,z,crs){
    list(nn = x,
         point = sf::st_sfc(y, crs = crs),
         lines = z[x])
  })
  input <- unname(input)

  message(paste0(Sys.time()," measuring exact distances for nearest ",k," lines"))
  cl <- parallel::makeCluster(ncores)
  result <- pbapply::pblapply(input,
                           FUN = nn_int,
                           cl = cl)

  parallel::stopCluster(cl)
  rm(cl)

  idx <- unlist(lapply(result, `[[`, 1))
  dist <- unlist(lapply(result, `[[`, 2))
  res <- list(idx = idx, dist = dist)
  return(res)
}

#' Internal Function
#'
#' @param sub list of lists
#' @noRd
#' @family internal
#' @return list
#' @details for checking line point distance
#'
nn_int <- function(sub){
  dists <- as.numeric(sf::st_distance(sub$point, sub$lines))
  dists_min <- min(dists)
  idx <- sub$nn[dists == dists_min]
  idx <- idx[1] # some case of equal distance
  res <- list(idx = idx,
              dist = dists_min)
  return(res)

}


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
