#### AWS RDS Testing ####
library(tidyverse)
library(RMySQL)
library(RMariaDB)
library(readr)
library(dbplyr)
library(sf)
library(aws.s3)
library(RPostgreSQL)


host <- "epic-test-db.clowwyayar55.us-east-1.rds.amazonaws.com"
port <- 3306
user <- "admin"
password <- "epictestdb"
dbname <- "testdb"

#postgresadmin
#epictestdb
#testdb

# Establish database connection
con <- dbConnect(
  drv = MariaDB(),
  dbname = dbname,
  host = host,
  port = port,
  user = user,
  password = password
)

dbGetQuery(con, "SELECT VERSION()")

## Creating sample data 
# super_sleepers <- data.frame(rating=1:4,
#                              animal=c('koala', 'hedgehog', 'sloth', 'panda'),
#                              country=c('Australia', 'Italy', 'Peru', 'China'),
#                              avg_sleep_hours=c(21, 18, 17, 10))

write.csv(super_sleepers, "super_sleepers.csv", row.names = FALSE)

data <- read.csv("super_sleepers.csv", stringsAsFactors = FALSE)

# Specify the table name
table_name <- "your_table_name"

# Write the data into the database
dbWriteTable(con, table_name, data, row.names = FALSE, overwrite = TRUE)

# Query to retrieve data from the table
query <- paste("SELECT * FROM", table_name)

# Execute the query and fetch the data
result <- dbGetQuery(con, query)

# Close the connection
dbDisconnect(con)

## Testing dbplyr ## 
db <- tbl(con, table_name)

result <- db %>%
  filter(animal == "koala") %>%
  collect()
# ^ This works!

dbDisconnect(con)

## Testing GeoJSON and RPostgreSQL as we need geojson support..
# importing from S3 


tx_sab_geo <- aws.s3::s3read_using(st_read, 
                                   object = "state-drinking-water/TX/clean/app/tx_sab_simplified.geojson",
                                   bucket = "tech-team-data")

tx_data_test <- aws.s3::s3read_using(st_read, 
                                     object = "state-drinking-water/TX/clean/app/tx_sab_simplified.geojson",
                                     bucket = "tech-team-data")

tx_data_test <- st_read(tx_data_test)

tx_spatial_test <- tx_data_test %>%
                    select(pwsid, geometry)

st_write(tx_spatial_test, "tx_spatial_test.geojson")

tmp <- tempfile()
st_write(tx_spatial_test, dsn = paste0(tmp, ".geojson"))
on.exit(unlink(tmp))

put_object(
  file = "super_sleepers.csv",
  object = "/state-drinking-water/TX/clean/app/app_test_data_spatial.csv",
  bucket = "tech-team-data",
 # acl = "public-read",
  multipart = TRUE
)


# Set up database connection parameters
host <- "epic-test-postgre.clowwyayar55.us-east-1.rds.amazonaws.com"
port <- 5432  # default PostgreSQL port
dbname <- "test-db-postgre"
user <- "postgres"
password <- "epictestdb"


# Connect to PostgreSQL RDS
con <- dbConnect(drv = PostgreSQL(), 
                 host = host, 
                 port = port,
                 dbname = dbname, 
                 user = user, 
                 password = password)

