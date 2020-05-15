Generate vector tiles for London data
================
Institute for Transport Studies, University of Leeds

### Got the geojson already

``` r
# private so download yourself
u = "https://github.com/saferactive/cyipt/raw/master/london_cyipt.zip"
f = "ignored/london_cyipt.zip"
d = "ignored"
geojson = file.path(d, "london_cyipt.geojson")
if(!file.exists(d)) {
  dir.create(d)
}
# private so download yourself but for future
# download.file(u, destfile = f)
# unzip(f, exdir = d)
stopifnot(file.exists(geojson))
```

## Mbtiles

Generating the mapbox tiles is a must:

### Install tippecanoe

Copy instructins from source:

OSX with Homebrew:

    $ brew install tippecanoe

On Ubuntu it will usually be easiest to build from the source
repository:

    $ git clone https://github.com/mapbox/tippecanoe.git
    $ cd tippecanoe
    $ make -j
    $ make install

### Generate the tiles

Could run the command from R:

``` r
system("tippecanoe -zg -o ignored/london.mbtiles --drop-densest-as-needed ignored/london_cyipt.geojson")
# or in a terminal
# tippecanoe -zg -o ignored/out.mbtiles --drop-densest-as-needed london_cyipt.geojson
```

### Serving

#### Usee mbview repo

``` r
mbd = "ignored/mbview"
if(file.exists()) {
  system("git clone https://github.com/mapbox/mbview.git ignored/mbview")
}
cw = getwd()
setwd(mbd)
system("npm i")
# one downside is forcing us to use a mapbox token
system(paste0("export MAPBOX_ACCESS_TOKEN=", Sys.getenv("MAPBOX_ACCESS_TOKEN")))
system("node cli.js --port 9000 ../london.mbtiles")
# opens localhost:9000
setwd(cw)
```

<img width="100%" alt="Screenshot 2020-05-15 at 13 18 48" src="https://user-images.githubusercontent.com/408568/82049801-e6455200-96ae-11ea-8e14-7863845aac0b.png">

### styling (json)

### Shiny, eatlas or custom (simple)
