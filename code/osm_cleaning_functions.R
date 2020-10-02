# Functions to subset to main roads in osm
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

osm_consolidate <- function(x, segment = 500){
  x = x[,c("name", "ref", "highway")]
  # Group
  x <- dplyr::group_by(x, name, ref, highway)
  x <- dplyr::summarise(x)
  # TODO: check st_combine against st_union for dplyr::summarise(x)
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


cluster_junction <- function(x){
  buff <- sf::st_buffer(x, dist = 15, nQuadSegs = 15)
  ints <- sf::st_intersects(buff)
  message("Clustering Junctions")
  ints_clus <- cluster_ints(ints)
  ints_clus <- ints_clus[lengths(ints_clus) > 0]

  message("Creating Geometries")
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
  return(geoms)
}

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


nn_line <- function(point, lines, k = 20){
  cents <- sf::st_centroid(lines)
  nn <- nngeo::st_nn(point, cents, k = k)

  res <- list()
  pb <- progress_bar$new(total = nrow(point))
  for(i in seq_len(nrow(point))){
    pb$tick()
    nnsub <- nn[[i]]
    suppressMessages(sub <- unlist(nngeo::st_nn(point$geometry[i], sf::st_geometry(lines)[nnsub], progress = FALSE)))
    res[[i]] <- nnsub[sub]
  }
  res <- unlist(res)
  return(res)

}


nn_line2 <- function(point, lines, k = 50, ncores = 1){
  cents <- sf::st_centroid(lines)
  message(paste0(Sys.time()," finding nearest ",k," centroids"))
  nn <- nngeo::st_nn(point, cents, k = k, parallel = ncores)
  lines <- sf::st_geometry(lines)
  message(paste0(Sys.time()," splitting results"))
  lines_lst <- pbapply::pblapply(nn, function(x){lines[x]})
  point <- sf::st_geometry(point)
  point_lst <- split(point, seq_len(length(point)))

  message(paste0(Sys.time()," finding nearest 1 of ",k," lines"))
  cl <- parallel::makeCluster(ncores)
  res <- pbapply::pbmapply(FUN = nn_int,
                           pt = point_lst,
                           lns = lines_lst,
                           nnl = nn,
                           USE.NAMES = FALSE)
  parallel::stopCluster(cl)
  rm(cl)

  # qtm(point[1:100,]) +
  #   qtm(lines[res,])


  # res <- list()
  # pb <- progress_bar$new(total = nrow(point))
  # for(i in seq_len(nrow(point))){
  #   pb$tick()
  #   nnsub <- nn[[i]]
  #   suppressMessages(sub <- unlist(nngeo::st_nn(point$geometry[i], sf::st_geometry(lines)[nnsub], progress = FALSE)))
  #   res[[i]] <- nnsub[sub]
  # }
  # res <- unlist(res)
  return(res)

}

nn_int <- function(pt, lns, nnl){
  suppressMessages(sub <- unlist(nngeo::st_nn(pt, lns, progress = FALSE)))
  nnl[sub]
  return(nnl[sub])
}

line_segment_sf <- function(l, n_segments, segment_length = NA) {
  if (!is.na(segment_length)) {
    l_length <- as.numeric(sf::st_length(l))
    n_segments <- round(l_length / segment_length)
  }

  attrib <- sf::st_drop_geometry(l)
  geom <- sf::st_geometry(l)

  attrib <- attrib[rep(seq(1,nrow(attrib)), times = n_segments),]

  split_int <- function(i){
    ln <- geom[i]
    n <- n_segments[i]
    pts <- sf::st_cast(sf::st_sfc(ln),"POINT")
    lth <- length(pts)
    brks <- seq_len(lth)[!duplicated(ceiling(seq_len(lth)/(lth/n)))]
    brks <- brks[seq(2,length(brks))]
    pts <- pts[brks]
    res <- lwgeom::st_split(ln, pts)
    res <- sf::st_collection_extract(res, "LINESTRING")
    return(res)
  }

  geom <- pbapply::pblapply(seq(1, length(geom)),
                           FUN = split_int)
  geom <- unlist(geom, recursive = FALSE)
  geom <- st_as_sfc(geom)

  st_geometry(attrib) <- geom
  st_crs(attrib) <- st_crs(l)

  return(attrib)
}



