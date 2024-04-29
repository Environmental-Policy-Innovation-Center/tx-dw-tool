### simple data prep and pushing for basemap geojsons ### 


library(sf)
library(rgdal)
library(aws.s3)
library(tidyverse)

## These files were simplified using mapshapper
## TO DO: change to repository folder file paths 
tx_sab <- st_read("/Users/gw/Downloads/tx_sab_super_simplified.geojson.json")

tx_counties <- st_read("/Users/gw/Downloads/tx_counties_v3.json")%>%
                select(namelsad)

tx_regions <- st_read("/Users/gw/Downloads/TX_regions_simplified.json")%>%
               select(label_2)


tmp <- tempfile()
st_write(tx_sab, dsn = paste0(tmp, ".geojson"))
on.exit(unlink(tmp))
put_object(
  file = paste0(tmp, ".geojson"),
  object = "/state-drinking-water/TX/clean/app/tx_sab_super_simplified.geojson",
  bucket = "tech-team-data",
  acl = "public-read"
)

tmp <- tempfile()
st_write(tx_counties, dsn = paste0(tmp, ".geojson"))
on.exit(unlink(tmp))
put_object(
  file = paste0(tmp, ".geojson"),
  object = "/state-drinking-water/TX/clean/app/tx_counties_simplified_v2.geojson",
  bucket = "tech-team-data",
  acl = "public-read"
)

tmp <- tempfile()
st_write(tx_regions, dsn = paste0(tmp, ".geojson"))
on.exit(unlink(tmp))
put_object(
  file = paste0(tmp, ".geojson"),
  object = "/state-drinking-water/TX/clean/app/tx_regions_simplified.geojson",
  bucket = "tech-team-data",
  acl = "public-read"
)
