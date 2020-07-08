

```r
# Aim: create walking route networks from PCT data


# test on small region ----------------------------------------------------

region_name = "isle-of-wight"
r = pct::get_pct_routes_fast(region = region_name, geography = "lsoa")
rnet_walking = stplanr::overline(r, attrib = "foot")
```

```
## 2020-07-03 11:47:54 constructing segments
```

```
## 2020-07-03 11:47:55 building geometry
```

```
## 2020-07-03 11:47:56 simplifying geometry
```

```
## 2020-07-03 11:47:56 aggregating flows
```

```
## 2020-07-03 11:47:56 rejoining segments into linestrings
```

```r
plot(rnet_walking)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
# for london --------------------------------------------------------------

region_name = "london"
r = pct::get_pct_routes_fast(region = region_name, geography = "lsoa")
rnet_walking = stplanr::overline(r, attrib = "foot")
```

```
## 2020-07-03 11:50:12 constructing segments
```

```
## 2020-07-03 11:51:17 building geometry
```

```
## 2020-07-03 11:51:27 simplifying geometry
```

```
## large data detected, using regionalisation, nrow = 400111
```

```
## although coordinates are longitude/latitude, st_intersects assumes that they are planar
## although coordinates are longitude/latitude, st_intersects assumes that they are planar
## although coordinates are longitude/latitude, st_intersects assumes that they are planar
```

```
## 2020-07-03 11:51:41 regionalisation complete, aggregating flows
```

```
## 2020-07-03 11:51:50 rejoining segments into linestrings
```

```r
plot(rnet_walking)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png)

```r
f = paste0("rnet_walking_", region_name, ".Rds")
saveRDS(rnet_walking, f)
piggyback::pb_upload(f)
```

```
## uploading rnet_walking_london.Rds ...
```

```
## 
```

```r
piggyback::pb_download_url(f)
```

```
## [1] "https://github.com/saferactive/saferactive/releases/download/0.1/rnet_walking_london.Rds"
```

```r
# to run this from bash and report results:
# knitr::spin("code/walking-rnet.R")
# from bash
# Rscript -e 'knitr::spin("code/walking-rnet.R")'
# file.rename("walking-rnet.md", "code/walking-rnet.md")
```
