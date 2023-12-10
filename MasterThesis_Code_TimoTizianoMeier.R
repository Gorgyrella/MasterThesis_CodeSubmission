##########################################################################
################## Master Thesis Timo Meier ##############################
##########################################################################

##########################################################
################## How to run the code ###################
##########################################################

# STEP 1: Download the RScript and all attached files somewhere on your desktop. The code is written
#         in a way that you should be able to run it without having to adjust any file path names.

# STEP 2: To access the two APIs used throughout this thesis, you will need two API keys:
#             2.1) Bings Maps API Key: https://learn.microsoft.com/en-us/bingmaps/getting-started/bing-maps-dev-center-help/getting-a-bing-maps-key#creating-a-bing-maps-key
#                  Make sure to get an Education Key through HSG so there are no request limits.
#             2.2) EventStudyTools API Key: https://www.eventstudytools.com/api-access
#                  The key for this powerful Event Study API is 10.- USD and valid for a month, which is great 
#                  value for money in my opinion.

# STEP 3: Adjust the parameters to reproduce the desired outcome from the thesis. E.g., adjust the event windows
#         to your desired model. Spots where adjustments are needed are clearly marked and the necessary adjustments
#         are outlined throughout the code in the following format:
# --------------------> SWITCH: DESCRIPTION OF NECESSARY ADJUSTMENT #####

##########################################################
################## Clean R ###############################
##########################################################

# The getwd function shows your current working directory
getwd()

# This function lets you set your working directory to wherever you're saving this R script
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# We create a vector with some packages that we will need for this project
pkgs <- c("EventStudy",
          "eventstudies",
          "xtable",
          "zoo",
          "igraph",
          "rnaturalearth", 
          "rnaturalearthdata", 
          "geosphere", 
          "purrr",
          "lwgeom", 
          "lubridate",
          "digest", 
          "githubinstall", 
          "devtools", 
          "jsonlite",
          "httr",
          "sf", 
          "stringi", 
          "Matrix", 
          "lmtest", 
          "dplyr", 
          "plm", 
          "lfe", 
          "car", 
          "faux", 
          "ggplot2", 
          "tidyr", 
          "mvtnorm", 
          "fBasics", 
          "rstudioapi", 
          "AER", 
          "dynlm", 
          "forecast", 
          "readxl", 
          "stargazer", 
          "scales", 
          "quantmod", 
          "tidyverse", 
          "urca", 
          "sandwich", 
          "tseries", 
          "astsa", 
          "stats"
          )

devtools::install_github("EventStudyTools/api-wrapper.r"
                         #, force = T
)

# We create a loop which will go through each of the packages
for(pkg in pkgs){
  
  # In case you have never used a certain package, you first need to install it. To do that, simply remove the
  # hashtag in front of the line of code below before you run it
  # install.packages(pkg,character.only = TRUE)
  
  library(pkg, 
          character.only = T)}

# Cean the global environment before really getting started
rm(list=ls())

# --------------------> SWITCH: INSERT YOUR BING MAPS API KEY
bing.maps.API.key <- "Ah0egUm2rM71pFDforim-4vceg1ozsPalS-AHCmTArEJpn-hHKvCPpg_0KH6OCOd"

# --------------------> SWITCH: INSERT YOUR EVENTSTUDYTOOLS API KEY
eventstudytools.API.key <- "YOUR KEY"

##########################################################
################## Load data #############################
#########################################################

########################################
##### Compustat Historcial Segments ####
########################################

# Load all data from Compustat
All_Compustat <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230605_All Compustat Historical Segments.csv"), 
                 header = T,
                 sep = ",")

# We get a brief overview of the data
summary(All_Compustat)
# >>> We notice that the values for the "date" column are incorrectly formatted

# We reformat the date column
All_Compustat$srcdate <- as.Date(All_Compustat$srcdate, tryFormats = c("%Y-%m-%d"))
All_Compustat$srcdate.year <- year(All_Compustat$srcdate)

# Rename all columns starting with s. for supplier information and c. for customer information
All_Compustat <- All_Compustat %>% 
  rename(s.gvkey = gvkey,
         c.cid = cid,
         c.name = cnms,
         c.ctype = ctype,
         c.gareac = gareac,
         c.gareat = gareat,
         s.salecs = salecs,
         s.sid = sid,
         s.stype = stype,
         s.srcdate = srcdate,
         s.srcdate.year = srcdate.year,
         s.name = conm,
         s.tic = tic,
         s.cusip = cusip,
         s.cik = cik,
         s.sic = sic,
         s.naics = naics)

# Convert supplier and customer names to lower case
All_Compustat$s.name <- stri_trans_tolower(All_Compustat$s.name, locale = "en_US.UTF-8")
All_Compustat$c.name <- stri_trans_tolower(All_Compustat$c.name, locale = "en_US.UTF-8")

# Delete observations with no sales to customers
Cleaned_Compustat <- subset(All_Compustat, !is.na(s.salecs))
Cleaned_Compustat <- subset(Cleaned_Compustat, s.salecs >= 0)

# Load link file
Link.gvkey <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230605_Link by GVKEY.csv"), 
                       header = T,
                       sep = ",")

# Convert company names to lower case
Link.gvkey$conm <- stri_trans_tolower(Link.gvkey$conm, locale = "en_US.UTF-8")

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(Link.gvkey$gvkey, Link.gvkey$conm, sep = ", "))

# Bind the new row to the original merged data frame
Link.gvkey <- cbind(Link.gvkey, remove.duplicates)

# Sort the dataframe by remove.duplicates and LINKTYPE
Link.gvkey <- Link.gvkey %>%
  arrange(remove.duplicates, LINKTYPE)

# Keep the first row of each remove.duplicates
Link.gvkey <- Link.gvkey %>%
  distinct(remove.duplicates, .keep_all = TRUE)

# Check if there's still duplicates in remove.duplicates
if(any(duplicated(Link.gvkey$remove.duplicates))) {
  # If there are, keep the one with LINKTYPE == "LC"
  Link.gvkey <- Link.gvkey %>%
    group_by(remove.duplicates) %>%
    filter(LINKTYPE == "LC" | n() == 1) %>%
    ungroup()
}

Link.gvkey <- Link.gvkey[, c("gvkey", "conm", "sic")]

# Add column with gvkey identifier to data
Cleaned_Compustat <- merge(Cleaned_Compustat, Link.gvkey, by.x = c("c.name"), by.y = c("conm"), all.x = TRUE)

Cleaned_Compustat <- Cleaned_Compustat %>% 
  rename(c.gvkey = gvkey,
         c.sic = sic
         )

# Remove columns where there is no GVKEY identifier for suppliers
Cleaned_Compustat <- subset(Cleaned_Compustat, !is.na(s.gvkey))

# Remove columns where there is no namer for suppliers
Cleaned_Compustat <- subset(Cleaned_Compustat, !is.na(s.name))

# Remove columns where there is no gvkey identifier for customers
Cleaned_Compustat <- subset(Cleaned_Compustat, !is.na(c.gvkey))

# Remove columns where there is no name for customers
Cleaned_Compustat <- subset(Cleaned_Compustat, !is.na(c.name))

length(unique(Cleaned_Compustat$c.gvkey))

##### Obtain supplier addresses and exclude non-US suppliers #####

# Load supplier locations data
All_HQ_addresses <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230614_All HQ addresses.csv"), 
                                  header = T,
                                  sep = ",")
  
# We reformat the date column
All_HQ_addresses$datadate <- as.Date(All_HQ_addresses$datadate, tryFormats = c("%Y-%m-%d"))
All_HQ_addresses$datadate.year <- year(All_HQ_addresses$datadate)

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(All_HQ_addresses$gvkey, All_HQ_addresses$datadate.year, sep = ", "))

# Bind the new row to the original merged data frame
All_HQ_addresses <- cbind(All_HQ_addresses, remove.duplicates)

All_HQ_addresses <- All_HQ_addresses[!duplicated(All_HQ_addresses$remove.duplicates), ]

All_HQ_addresses <- All_HQ_addresses[, c("datadate.year",
                                                   "gvkey", 
                                                   "add1", 
                                                   "add2", 
                                                   "add3", 
                                                   "add4", 
                                                   "addzip",
                                                   "city",
                                                   "idbflag",
                                                   "loc",
                                                   "state"
                                                   )]

# Create a new row with the concatenated values
Cleaned_Compustat <- merge(Cleaned_Compustat, All_HQ_addresses, by.x = c("s.srcdate.year", "s.gvkey"), by.y = c("datadate.year", "gvkey"), all.x = TRUE)

# Remove non-US suppliers
Cleaned_Compustat <- subset(Cleaned_Compustat, loc == "USA")

# Remove non-domestic suppliers
Cleaned_Compustat <- subset(Cleaned_Compustat, idbflag == "D")

# Remove observations before 1985
Cleaned_Compustat <- subset(Cleaned_Compustat, s.srcdate >= "1985-01-01")

# Remove observations after 2022
Cleaned_Compustat <- subset(Cleaned_Compustat, s.srcdate <= "2022-12-31")

# Rename all columns starting with s. for supplier information and c. for customer information
Cleaned_Compustat <- Cleaned_Compustat %>% 
  rename(s.add1 = add1,
         s.add2 = add2,
         s.add3 = add3,
         s.add4 = add4,
         s.addzip = addzip,
         s.city = city,
         s.idbflag = idbflag,
         s.loc = loc,
         s.state = state
  )


# Create a new row with the concatenated values
s.address <- data.frame(s.address = paste(Cleaned_Compustat$s.add1,
                                          Cleaned_Compustat$s.city,
                                          Cleaned_Compustat$s.state,
                                          Cleaned_Compustat$s.addzip,
                                          Cleaned_Compustat$s.loc,
                                          sep = ", "
                                          )
                        )

# Bind the new row to the original merged data frame
Cleaned_Compustat <- cbind(Cleaned_Compustat, s.address)

##### Obtain customer addresses and exclude non-US customers #####

# Create a new row with the concatenated values
Cleaned_Compustat <- merge(Cleaned_Compustat, All_HQ_addresses, by.x = c("s.srcdate.year", "c.gvkey"), by.y = c("datadate.year", "gvkey"), all.x = TRUE)

# Remove observations before 1985
Cleaned_Compustat <- subset(Cleaned_Compustat, s.srcdate >= "1985-01-01")

# Remove observations after 2022
Cleaned_Compustat <- subset(Cleaned_Compustat, s.srcdate <= "2022-12-31")

# Rename all columns starting with s. for supplier information and c. for customer information
Cleaned_Compustat <- Cleaned_Compustat %>% 
  rename(c.add1 = add1,
         c.add2 = add2,
         c.add3 = add3,
         c.add4 = add4,
         c.addzip = addzip,
         c.city = city,
         c.idbflag = idbflag,
         c.loc = loc,
         c.state = state
  )

# Remove non-US customers
Cleaned_Compustat <- subset(Cleaned_Compustat, c.loc == "USA")

# Remove non-domestic customers
Cleaned_Compustat <- subset(Cleaned_Compustat, c.idbflag == "D")


# Create a new row with the concatenated values
c.address <- data.frame(c.address = paste(Cleaned_Compustat$c.add1,
                                          Cleaned_Compustat$c.city,
                                          Cleaned_Compustat$c.state,
                                          Cleaned_Compustat$c.addzip,
                                          Cleaned_Compustat$c.loc,
                                          sep = ", "
)
)

# Bind the new row to the original merged data frame
Cleaned_Compustat <- cbind(Cleaned_Compustat, c.address)

length(unique(Cleaned_Compustat$s.gvkey))
length(unique(Cleaned_Compustat$c.gvkey))

########################################
##### Bing Maps API ####################
########################################

##### Obtainsupplier address coordinates #####

# Add latitude and longitude columns
Cleaned_Compustat$s.latitude <- NA
Cleaned_Compustat$s.longitude <- NA

# Initialize a data frame to store failed addresses
Failed_Geocoding_Suppliers <- data.frame(
  index = integer(),
  address = character(),
  stringsAsFactors = FALSE
)

# Define the geocode_address function
geocode_address <- function(address) {
  base_url <- "http://dev.virtualearth.net/REST/v1/Locations"
  query_params <- list(
    query = address,
    key = bing.maps.API.key
  )
  response <- GET(url = base_url, query = query_params)
  if (http_type(response) == "application/json") {
    result <- content(response, as = "parsed")
    if (result$statusDescription == "OK" && result$resourceSets[[1]]$estimatedTotal > 0) {
      coordinates <- result$resourceSets[[1]]$resources[[1]]$point$coordinates
      return(coordinates)
    }
  }
  return(NULL)
}

# Loop through the addresses and geocode them
for (i in 1:nrow(Cleaned_Compustat)) {
  address <- Cleaned_Compustat$s.address[i]
  
  # Print out the current index and address
  cat("Processing supplier address", i, ":", address, "\n")
  
  coordinates <- geocode_address(address)
  if (!is.null(coordinates)) {
    latitude <- coordinates[1]
    longitude <- coordinates[2]
    Cleaned_Compustat$s.latitude[i] <- latitude
    Cleaned_Compustat$s.longitude[i] <- longitude
  } else {
    Cleaned_Compustat$s.latitude[i] <- NA
    Cleaned_Compustat$s.longitude[i] <- NA
    cat("Geocoding failed for the address:", address, "\n")
    
    # Add the failed address to the dataframe
    Failed_Geocoding_Suppliers <- rbind(Failed_Geocoding_Suppliers, data.frame(index = i, address = address))
  }
}

##### Obtain customer address coordinates ##### #####

# Add latitude and longitude columns
Cleaned_Compustat$c.latitude <- NA
Cleaned_Compustat$c.longitude <- NA

# Initialize a data frame to store failed addresses
Failed_Geocoding_Customers <- data.frame(
  index = integer(),
  address = character(),
  stringsAsFactors = FALSE
)

# Define the geocode_address function
geocode_address <- function(address) {
  base_url <- "http://dev.virtualearth.net/REST/v1/Locations"
  query_params <- list(
    query = address,
    key = bing.maps.API.key
  )
  response <- GET(url = base_url, query = query_params)
  if (http_type(response) == "application/json") {
    result <- content(response, as = "parsed")
    if (result$statusDescription == "OK" && result$resourceSets[[1]]$estimatedTotal > 0) {
      coordinates <- result$resourceSets[[1]]$resources[[1]]$point$coordinates
      return(coordinates)
    }
  }
  return(NULL)
}

# Loop through the addresses and geocode them
for (i in 1:nrow(Cleaned_Compustat)) {
  address <- Cleaned_Compustat$c.address[i]
  
  # Print out the current index and address
  cat("Processing customer address", i, ":", address, "\n")
  
  coordinates <- geocode_address(address)
  if (!is.null(coordinates)) {
    latitude <- coordinates[1]
    longitude <- coordinates[2]
    Cleaned_Compustat$c.latitude[i] <- latitude
    Cleaned_Compustat$c.longitude[i] <- longitude
  } else {
    Cleaned_Compustat$c.latitude[i] <- NA
    Cleaned_Compustat$c.longitude[i] <- NA
    cat("Geocoding failed for the address:", address, "\n")
    
    # Add the failed address to the dataframe
    Failed_Geocoding_Customers <- rbind(Failed_Geocoding_Customers, data.frame(index = i, address = address))
  }
}

Compustat_HQ <- Cleaned_Compustat

Compustat_HQ$s.longitude <- unlist(Compustat_HQ$s.longitude)
Compustat_HQ$s.latitude <- unlist(Compustat_HQ$s.latitude)
Compustat_HQ$c.longitude <- unlist(Compustat_HQ$c.longitude)
Compustat_HQ$c.latitude <- unlist(Compustat_HQ$c.latitude)

# Here I decided to save a legacy copy
# write.csv(Compustat_HQ, file = file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230912_Compustat_HQ.csv"))

Compustat_HQ <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230912_Compustat_HQ.csv"), 
                         header = T,
                         sep = ",")

map.addresses <- Compustat_HQ
Compustat_Customers <- Compustat_HQ

Compustat_HQ$s.longitude <- unlist(Compustat_HQ$s.longitude)
Compustat_HQ$s.latitude <- unlist(Compustat_HQ$s.latitude)
Compustat_HQ <- subset(Compustat_HQ, select = -c.latitude)
Compustat_HQ <- subset(Compustat_HQ, select = -c.longitude)

Compustat_Customers$c.longitude <- unlist(Compustat_Customers$c.longitude)
Compustat_Customers$c.latitude <- unlist(Compustat_Customers$c.latitude)
Compustat_Customers <- subset(Compustat_Customers, select = -s.latitude)
Compustat_Customers <- subset(Compustat_Customers, select = -s.longitude)

length(unique(Compustat_HQ$s.gvkey))
length(unique(Compustat_HQ$c.gvkey))

##### Relationships over the years graph #####

# Count observations per year
yearly_counts <- table(Compustat_HQ$s.srcdate.year)

# Convert to a data frame for ggplot2
df <- as.data.frame(yearly_counts)
colnames(df) <- c("Year", "Count")

# Plot histogram using ggplot2
p <- ggplot(Compustat_HQ, aes(x=s.srcdate.year)) + 
  geom_histogram(fill="grey90", color="black", binwidth=1) + # Setting binwidth=1 ensures one bar per year
  labs(#title="Histogram of observations per year", 
       x="Year", 
       y="# of observations") +
  theme_minimal() + 
  theme(text = element_text(size=20),
        axis.text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # axis.title.x = element_text(vjust=-0.9),  # Adjust this value as needed; negative values move the label down
        # axis.title.y = element_text(hjust=-0.9)   # Adjust this value as needed; negative values move the label to the left
        )

print(p)

##### Trade flows graph #####

Compustat_HQ_graph <- Compustat_HQ

# Extract 2-digit SIC code
Compustat_HQ_graph$s.sic <- substr(Compustat_HQ_graph$s.sic, 1, 2)
Compustat_HQ_graph$c.sic <- substr(Compustat_HQ_graph$c.sic, 1, 2)

# Map SIC codes to industry names
Compustat_HQ_graph$s.industry <- cut(as.integer(Compustat_HQ_graph$s.sic), breaks=c(0, 9, 14, 17, 19, 39, 49, 51, 59, 67, 89, 97, 99),
                               labels=c("Agriculture,; Forestry,; Fishing", "Mining", "Construction", "not used", "Manufacturing", "Transport,; Comms,; Electric, Gas,; Sanitary serv.", "Wholesale; Trade", "Retail; Trade", "Finance,; Insurance,; Real Estate", "Services", "Public Administration", "Nonclassifiable"))
Compustat_HQ_graph$c.industry <- cut(as.integer(Compustat_HQ_graph$c.sic), breaks=c(0, 9, 14, 17, 19, 39, 49, 51, 59, 67, 89, 97, 99),
                               labels=c("Agriculture,; Forestry,; Fishing", "Mining", "Construction", "not used", "Manufacturing", "Transport,; Comms,; Electric, Gas,; Sanitary serv.", "Wholesale; Trade", "Retail; Trade", "Finance,; Insurance,; Real Estate", "Services", "Public Administration", "Nonclassifiable"))

# Replace ", " with "\n" in industry names
Compustat_HQ_graph$s.industry <- str_replace_all(Compustat_HQ_graph$s.industry, "; ", "\n")
Compustat_HQ_graph$c.industry <- str_replace_all(Compustat_HQ_graph$c.industry, "; ", "\n")

# Define industry order
industry_order <- c("Agriculture,; Forestry,; Fishing", "Mining", "Construction", "not used", "Manufacturing", "Transport,; Comms,; Electric, Gas,; Sanitary serv.", "Wholesale; Trade", "Retail; Trade", "Finance,; Insurance,; Real Estate", "Services", "Public Administration", "Nonclassifiable")

# Replace ", " with "\n" in industry order
industry_order <- str_replace_all(industry_order, "; ", "\n")

# Create factors with specific order
Compustat_HQ_graph$s.industry <- factor(Compustat_HQ_graph$s.industry, levels = industry_order)
Compustat_HQ_graph$c.industry <- factor(Compustat_HQ_graph$c.industry, levels = industry_order)

# Aggregate data based on the number of relationships
aggregated_data <- Compustat_HQ_graph %>%
  group_by(s.industry, c.industry) %>%
  summarise(num_relationships = n())

# Create graph object
g <- graph_from_data_frame(aggregated_data, directed=TRUE)
E(g)$width <- E(g)$num_relationships / max(E(g)$num_relationships) * 10 # scale number of relationships to [0, 10] for edge width

# Remove isolated vertices
g <- delete_vertices(g, V(g)[degree(g)==0])

# Order vertices by industry
V(g)$order <- as.numeric(factor(V(g)$name, levels=industry_order))
g <- permute(g, order(V(g)$order))

# Create circular layout
layout <- layout_in_circle(g)

# Plot graph
plot(g, layout=layout, edge.width=E(g)$width, vertex.color="grey90", vertex.size=40,
     edge.arrow.size=0.25, edge.color="gray30",
     vertex.frame.color="gray50", vertex.label.color="black", 
     vertex.label.cex=0.95, # adjust this value to change the size of the labels
     edge.curved=0.2, 
     )

##### Matrix #####

# Find industries that have relationships in both directions
supplier_industries <- unique(aggregated_data$s.industry)
customer_industries <- unique(aggregated_data$c.industry)

# Use the union instead of intersection to ensure all unique industries are included
all_industries <- union(supplier_industries, customer_industries)

# Create an empty matrix with all industries
industry_matrix <- matrix(0, nrow = length(all_industries), ncol = length(all_industries),
                          dimnames = list(all_industries, all_industries))

# Fill the matrix with the number of relationships
for(i in 1:nrow(aggregated_data)) {
  row_industry <- as.character(aggregated_data$s.industry[i])
  col_industry <- as.character(aggregated_data$c.industry[i])
  num_relationships <- aggregated_data$num_relationships[i]
  
  if(row_industry %in% all_industries && col_industry %in% all_industries){
    industry_matrix[row_industry, col_industry] <- num_relationships
  }
}

# Print matrix as LaTeX
print(xtable(industry_matrix, digits = 0), type = "latex", file = "industry_matrix.tex")

########################################
##### Dartmouth Floods Observatory #####
########################################

floods <- st_read(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/FloodArchive_region.shp"))

floods_US <- subset(floods, COUNTRY %in% c("USA",
                                           "USA.",
                                           " USA",
                                           "USA ",
                                           "Canada and USA",
                                           "American Samoa",
                                           "Guam",
                                           "Puerto Rico"
                                           ))

# Assuming the CRS is WGS 84 (EPSG code: 4326)
floods_US <- st_set_crs(floods_US, 4326)

# Perform the transformation
floods_US <- st_transform(floods_US, 4326)  # Replace 4326 with the target CRS if necessary

# Calculate width and length
floods_US <- floods_US %>%
  mutate(width_km = map_dbl(geometry, ~st_bbox(.) %>% {.$xmax - .$xmin} * 111.32),  # Convert degree to km, approximately
         length_km = map_dbl(geometry, ~st_bbox(.) %>% {.$ymax - .$ymin} * 111.32)) # Convert degree to km, approximately

# Filter out polygons greater than 333km in either width or length but keep records where floods_US$GLIDENUMBE is not 0
floods_US <- floods_US %>%
  filter((width_km <= 333 & length_km <= 333) 
       | GLIDENUMBE != 0 & GLIDENUMBE != "NA"
       | DEAD > 0
       | DISPLACED > 0
         )

# Step 1: Convert `Compustat_HQ` to a spatial points dataframe
Compustat_HQ <- st_as_sf(Compustat_HQ, coords = c("s.longitude", "s.latitude"), crs = 4326)
Compustat_Customers <- st_as_sf(Compustat_Customers, coords = c("c.longitude", "c.latitude"), crs = 4326)

# Step 2: Extract year from the `Compustat_HQ$s.srcdate` and `floods_US$s.srcdate` columns
Compustat_HQ$f.year <- year(Compustat_HQ$s.srcdate)
Compustat_Customers$f.year <- year(Compustat_HQ$s.srcdate)
floods_US$YEAR <- year(floods_US$BEGAN)

# Set the CRS for Compustat_HQ
st_crs(Compustat_HQ) <- 4326

# Check the CRS of floods_US
crs_floods_US <- st_crs(floods_US)

# If floods_US doesn't have a CRS, set it manually
if (is.na(crs_floods_US)) {
  st_crs(floods_US) <- 4326
}

# Now, transform Compustat_HQ to match the CRS of floods_US if they are not the same
if (st_crs(Compustat_HQ) != st_crs(floods_US)) {
  Compustat_HQ <- st_transform(Compustat_HQ, st_crs(floods_US))
}

floods_US$geometry <- st_make_valid(floods_US$geometry)

# Check which polygons are invalid
invalid_geometries <- st_is_valid(floods_US$geometry, reason = TRUE)

# Print out the IDs of invalid polygons
cat("Invalid polygon IDs:\n")
print(floods_US$ID[invalid_geometries != "Valid Geometry"])

table(invalid_geometries)

# Filter out invalid polygons
floods_US <- floods_US[invalid_geometries == "Valid Geometry", ]

# Find intersections
intersections <- st_intersects(Compustat_HQ, floods_US)
intersections_customers <- st_intersects(Compustat_Customers, floods_US)

floods_US <- floods_US[, c("YEAR", 
                           "ID", 
                           "LONG", 
                            "LAT", 
                            "AREA", 
                            "BEGAN", 
                            "ENDED", 
                            "DEAD",
                            "DISPLACED",
                            "SEVERITY"
                            )]

# Rename all columns starting with s. for supplier information and c. for customer information
floods_US <- floods_US %>% 
                    rename(f.year = YEAR,
                           f.id = ID,
                           f.long = LONG,
                           f.lat = LAT,
                           f.area = AREA,
                           f.began = BEGAN,
                           f.ended = ENDED,
                           f.dead = DEAD,
                           f.displaced = DISPLACED,
                           f.severity = SEVERITY
                           )

# Perform a spatial join between `Compustat_HQ` and `floods_US` datasets
Compustat_HQ <- st_join(Compustat_HQ, floods_US, join = st_intersects)
Compustat_HQ_Suppliers_FloodsBackup <- Compustat_HQ

Compustat_Customers <- st_join(Compustat_Customers, floods_US, join = st_intersects)
Compustat_Customers_FloodsBackup <- Compustat_Customers

Compustat_HQ <- subset(Compustat_HQ, f.year.x == f.year.y)
Compustat_Customers <- subset(Compustat_Customers, f.year.x == f.year.y)

# create a new column f.duration by subtracting the dates
Compustat_HQ$f.duration <- as.numeric(Compustat_HQ$f.ended - Compustat_HQ$f.began)

#view(Compustat_HQ$f.duration)
summary(subset(Compustat_HQ, f.id %in% unique(Compustat_HQ$f.id)))

#view(Compustat_HQ$f.duration)
summary(Compustat_HQ$f.duration)

Compustat_Customers_Merge <- data.frame(
                                c.gvkey=rep(NA, length(Compustat_Customers$f.id)),
                                s.salecs=rep(NA, length(Compustat_Customers$f.id)), 
                                s.srcdate=rep(NA, length(Compustat_Customers$f.id)), 
                                f.id=rep(NA, length(Compustat_Customers$f.id)),
                                stringsAsFactors = FALSE
)

Compustat_Customers_Merge$c.gvkey <- Compustat_Customers$c.gvkey
Compustat_Customers_Merge$s.salecs <- Compustat_Customers$s.salecs
Compustat_Customers_Merge$s.srcdate <- Compustat_Customers$s.srcdate
Compustat_Customers_Merge$f.id <- Compustat_Customers$f.id

Compustat_HQ <- merge(Compustat_HQ, Compustat_Customers_Merge, by.x = c("s.srcdate", "c.gvkey", "s.salecs"), by.y = c("s.srcdate", "c.gvkey", "s.salecs"), all.x = TRUE
                      )

Compustat_HQ <- subset(Compustat_HQ, (is.na(f.id.x) | is.na(f.id.y)) | f.id.x != f.id.y)

Compustat_HQ <- subset(Compustat_HQ, select = -f.id.y)

Compustat_HQ <- Compustat_HQ %>% 
  rename(f.id = f.id.x
  )

# Create a new row with the concatenated values of all columns
remove.duplicates <- data.frame(remove.duplicates = apply(Compustat_HQ, 1, paste, collapse = ", "))

# Bind the new row to the original merged data frame
Compustat_HQ <- cbind(Compustat_HQ, remove.duplicates)

# Remove duplicated rows based on the 'remove.duplicates' column
Compustat_HQ <- Compustat_HQ[!duplicated(Compustat_HQ$remove.duplicates), ]

# Remove the 'remove.duplicates' column
Compustat_HQ <- subset(Compustat_HQ, select = -remove.duplicates)

########################################
##### CRSP/Compustat Merged ############
########################################

Compustat_HQ$f.eventid <- 1:length(Compustat_HQ$s.gvkey)

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(Compustat_HQ$s.name, Compustat_HQ$f.id, sep = ", "))

# Bind the new row to the original merged data frame
Compustat_HQ <- cbind(Compustat_HQ, remove.duplicates)

##### Supplier data cleansing #######

Compustat_HQ_Suppliers <- Compustat_HQ_Suppliers_FloodsBackup
Compustat_HQ_Suppliers <- subset(Compustat_HQ_Suppliers, s.gvkey %in% unique(Compustat_HQ$s.gvkey))
Compustat_HQ_Suppliers <- subset(Compustat_HQ_Suppliers, !f.id %in% unique(Compustat_HQ$f.id))

# Select only the required columns and filter out duplicates
unique_data_eventdates <- unique(Compustat_HQ[, c("s.name", "f.id", "f.began")])
unique_data_supplierdates <- unique(Compustat_HQ_Suppliers[, c("s.name", "f.id", "f.began")])

unique_data_eventdates$f.began <- as.Date(unique_data_eventdates$f.began, format = "%Y-%m-%d")
unique_data_supplierdates$f.began <- as.Date(unique_data_supplierdates$f.began, format = "%Y-%m-%d")

unique_data_eventdates$eventdatesofinterest <- "Yes"
unique_data_supplierdates$eventdatesofinterest <- "No"

unique_data <- rbind(unique_data_eventdates, unique_data_supplierdates)

# Convert the 'f.began' column to a Date type
unique_data$f.began <- as.Date(unique_data$f.began, format = "%Y-%m-%d")

# Calculate the difference in days for the same s.name
unique_data <- unique_data[order(unique_data$s.name, unique_data$f.began), ]
unique_data$days_diff <- ave(as.numeric(unique_data$f.began), unique_data$s.name, FUN = function(x) c(NA, diff(x)))

# Find s.name and f.id combinations where the difference is less than 180 days
to_exclude <- unique_data[!is.na(unique_data$days_diff) & unique_data$days_diff < 181 | unique_data$eventdatesofinterest == "No", c("s.name", "f.id")]

# Exclude combinations of s.name and f.id in to_exclude from Compustat_HQ
Compustat_HQ <- Compustat_HQ[!paste(Compustat_HQ$s.name, Compustat_HQ$f.id) %in% paste(to_exclude$s.name, to_exclude$f.id), ]

length(unique(Compustat_HQ$f.id))
length(unique(Compustat_HQ$s.gvkey))
length(unique(Compustat_HQ$c.gvkey))

Descriptive_Suppliers <- subset(Compustat_HQ, f.id %in% c(unique(Compustat_HQ$f.id)))
summary(Descriptive_Suppliers$f.duration)

Descriptive_Suppliers <- Descriptive_Suppliers[, c("f.duration", "f.dead", "f.displaced", "f.severity", "f.area")]
Descriptive_Suppliers$geometry <- NULL

stargazer(Descriptive_Suppliers,
          type = "latex",
          summary = T,
          summary.stat = c("N", "mean", "min", "p25", "median", "p75", "max", "sd")
)

length(unique(Compustat_HQ$f.id))
length(unique(Compustat_HQ$s.gvkey))
length(unique(Compustat_HQ$c.gvkey))

##### Flood graph #####

map.floods <- subset(floods_US, f.id %in% c(unique(Compustat_HQ$f.id)))
map.customerHQ <- subset(map.addresses, c.gvkey %in% c(unique(Compustat_HQ$c.gvkey)))
map.supplierHQ <- subset(map.addresses, s.gvkey %in% c(unique(Compustat_HQ$s.gvkey)))

map.floods <- st_as_sf(map.floods, coords = c("longitude", "latitude"), crs = 4326)
map.customerHQ <- st_as_sf(map.customerHQ, coords = c("c.longitude", "c.latitude"), crs = 4326)
map.supplierHQ <- st_as_sf(map.supplierHQ, coords = c("s.longitude", "s.latitude"), crs = 4326)

# Get the US map
map.US <- ne_states(country = "united states of america", returnclass = "sf")

bbox <- st_bbox(c(xmin = -124.848974, ymin = 24.396308, xmax = -66.934570, ymax = 49.384358), crs = st_crs(map.US))

map.US <- st_crop(map.US, bbox)

# Plot
ggplot() +
  geom_sf(data = map.US, fill = "grey95", color = "black") +
  geom_sf(data = map.floods, fill = "blue", alpha = 0.1) +
  #geom_sf(data = map.supplierHQ, color = "black", size = 3) +
  geom_sf(data = map.supplierHQ, color = "red2", size = 2) +
  #geom_sf(data = map.customerHQ, color = "black", size = 3) +
  geom_sf(data = map.customerHQ, color = "green3", size = 2) +
  theme_minimal() +
  theme(axis.text = element_text(size = 20))

Compustat_HQ <- subset(Compustat_HQ, select = -remove.duplicates)

# Filter out false observations SUPPLIERS
Compustat_HQ <- subset(Compustat_HQ, !f.eventid %in% c(
  1,5,6,24,41,145,146,187,221,222,223,226,242,244,256,312,318,327,352,397,470,503,540,251,452,492,545,258,548,550,551,552,553,9,19,20,22,23,47,57,88,92,96,115,135,137,179,183,193,200,206,207,208,218,231,234,235,243,247,253,255,271,272,273,275,276,277,278,281,282,283,298,306,311,313,317,337,347,396,422,449,450,468,474,491,498,499,535,549,252,324,362,432
))

length(unique(Compustat_HQ$f.id))
length(unique(Compustat_HQ$s.gvkey))
length(unique(Compustat_HQ$c.gvkey))

# Filter out statistically significant and negatively affected SUPPLIERS
Compustat_HQ <- subset(Compustat_HQ, f.eventid %in% c(
  27,28,37,38,43,48,52,90,102,131,142,151,262,268,287,297,302,378,379,423,441,476,500,526,544
))

length(unique(Compustat_HQ$f.id))
length(unique(Compustat_HQ$s.gvkey))
length(unique(Compustat_HQ$c.gvkey))

Compustat_HQ_NegAffSuppliersBackup <- Compustat_HQ

Compustat_Customers <- subset(Compustat_Customers, c.gvkey %in% unique(Compustat_HQ$c.gvkey))

##### Customer data cleansing #######

Compustat_Customers <- Compustat_Customers_FloodsBackup
Compustat_Customers <- subset(Compustat_Customers, c.gvkey %in% unique(Compustat_HQ$c.gvkey))

# Select only the required columns and filter out duplicates
unique_data_supplierdates <- unique(Compustat_HQ[, c("c.name", "f.id", "f.began")])
unique_data_customerdates <- unique(Compustat_Customers[, c("c.name", "f.id", "f.began")])

unique_data_customerdates$f.began <- as.Date(unique_data_customerdates$f.began, format = "%Y-%m-%d")
unique_data_supplierdates$f.began <- as.Date(unique_data_supplierdates$f.began, format = "%Y-%m-%d")

unique_data_supplierdates$eventdatesofinterest <- "Yes"
unique_data_customerdates$eventdatesofinterest <- "No"

unique_data <- rbind(unique_data_supplierdates, unique_data_customerdates)

# Convert the 'f.began' column to a Date type
unique_data$f.began <- as.Date(unique_data$f.began, format = "%Y-%m-%d")

# Calculate the difference in days for the same s.name
unique_data <- unique_data[order(unique_data$c.name, unique_data$f.began), ]
unique_data$days_diff <- ave(as.numeric(unique_data$f.began), unique_data$c.name, FUN = function(x) c(NA, diff(x)))

# Find s.name and f.id combinations where the difference is less than 180 days
to_exclude <- unique_data[!is.na(unique_data$days_diff) & unique_data$days_diff < 180 | unique_data$eventdatesofinterest == "No", c("c.name", "f.id")]

# Exclude combinations of s.name and f.id in to_exclude from Compustat_HQ
Compustat_HQ <- Compustat_HQ[!paste(Compustat_HQ$c.name, Compustat_HQ$f.id) %in% paste(to_exclude$c.name, to_exclude$f.id), ]

length(unique(Compustat_HQ$f.id))
length(unique(Compustat_HQ$s.name))
length(unique(Compustat_HQ$c.name))
length(unique(Compustat_HQ$f.eventid))

Descriptive_Customers <- subset(Compustat_HQ, f.id %in% c(unique(Compustat_HQ$f.id)))
summary(Descriptive_Customers$f.duration)

Descriptive_Customers <- Descriptive_Customers[, c("f.duration", "f.dead", "f.displaced", "f.severity", "f.area")]
Descriptive_Customers$geometry <- NULL

stargazer(Descriptive_Customers,
          type = "latex",
          summary = T,
          summary.stat = c("N", "mean", "min", "p25", "median", "p75", "max", "sd")
)

#view(Compustat_HQ$f.duration)
summary(subset(Compustat_HQ, f.id %in% unique(Compustat_HQ$f.id)))

#view(Compustat_HQ$f.duration)
summary(Compustat_HQ$f.duration)

##########################################################
################## Supplier Event Study ##################
##########################################################

# Get unique values from supplier column
unique_suppliers <- unique(Compustat_HQ$s.gvkey)

# Write the unique values to a plain text file
writeLines(as.character(unique_suppliers), "unique_suppliers.txt")

# Suppliers
Stock_Prices <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230912_Affected Suppliers Stock Prices.csv"), 
                                  header = T,
                                  sep = ",")

Stock_Prices$datadate <- as.Date(Stock_Prices$datadate, tryFormats = c("%Y-%m-%d"))

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(Stock_Prices$GVKEY, Stock_Prices$datadate, sep = ", "))

# Bind the new row to the original merged data frame
Stock_Prices <- cbind(Stock_Prices, remove.duplicates)

# Sort the dataframe by remove.duplicates and LINKTYPE
Stock_Prices <- Stock_Prices %>%
  arrange(remove.duplicates, LINKTYPE)

# Keep the first row of each remove.duplicates
Stock_Prices <- Stock_Prices %>%
  distinct(remove.duplicates, .keep_all = TRUE)

# Check if there's still duplicates in remove.duplicates
if(any(duplicated(Stock_Prices$remove.duplicates))) {
  # If there are, keep the one with LINKTYPE == "LC"
  Stock_Prices <- Stock_Prices %>%
    group_by(remove.duplicates) %>%
    filter(LINKTYPE == "LC" | n() == 1) %>%
    ungroup()
}

##### firmData #####

firmData <- data.frame(
                    FirmID = rep(NA, length(Stock_Prices$GVKEY)),
                    Date = rep(NA, length(Stock_Prices$GVKEY)),
                    ClosingPrice = rep(NA, length(Stock_Prices$GVKEY))
)

firmData <- data.frame(
  FirmID = Stock_Prices$conm,
  Date = format(Stock_Prices$datadate, "%d.%m.%Y"),
  ClosingPrice = Stock_Prices$prccd
)

# Convert customer names to lower case
firmData$FirmID <- stri_trans_tolower(firmData$FirmID, locale = "en_US.UTF-8")

# Exlcude dates with NA for Closing Price
firmData <- firmData[!is.na(firmData$ClosingPrice), ]

write.table(firmData, file = "02_firmData.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
            )

##### RequestFile #####

# Create an empty data frame with predefined column names
RequestFile <- data.frame(
                       EventID = rep(NA, length(Compustat_HQ$f.eventid)),
                       FirmID = rep(NA, length(Compustat_HQ$f.id)),
                       MarketID = rep(NA, length(Compustat_HQ$f.id)),
                       EventDate = rep(NA, length(Compustat_HQ$f.id)),
                       GroupingVariable = rep(NA, length(Compustat_HQ$f.id)),
                       StartEventWindow = rep(NA, length(Compustat_HQ$f.id)),
                       EndEventWindow = rep(NA, length(Compustat_HQ$f.id)),
                       EndofEstimationWindow = rep(NA, length(Compustat_HQ$f.id)),
                       EstimationWindowLength = rep(NA, length(Compustat_HQ$f.id)),
                       stringsAsFactors = FALSE
                       )

RequestFile <- data.frame(
                       EventID = Compustat_HQ$f.eventid,
                       FirmID = Compustat_HQ$s.name,
                       MarketID = "SP500",
                       EventDate = format(Compustat_HQ$f.began, "%d.%m.%Y"),
                       GroupingVariable = "None",
# --------------------> SWITCH: ADJUST EVENT WINDOW LENGTH AS DESIRED #####
                       StartEventWindow = -11,
                       EndEventWindow = 11,
                       EndofEstimationWindow = -12,
                       EstimationWindowLength = 120
                       )

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(RequestFile$FirmID, RequestFile$EventDate, sep = ", "))

# Bind the new row to the original merged data frame
RequestFile <- cbind(RequestFile, remove.duplicates)

RequestFile <- RequestFile[!duplicated(RequestFile$remove.duplicates), ]

RequestFile <- subset(RequestFile, select = -remove.duplicates)

length(unique(RequestFile$FirmID))
length(unique(firmData$FirmID))

write.table(RequestFile, file = "01_RequestFile.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
            )

##### MarketData #####

SP_500 <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230906_SP500.csv"), 
                            header = T,
                            sep = ",")

SP_500$caldt <- as.Date(SP_500$caldt, tryFormats = c("%Y-%m-%d"))

MarketData <- data.frame(MarketID = rep(NA, length(SP_500$caldt)),
                         Date = rep(NA, length(SP_500$caldt)),
                         ClosingPrice = rep(NA, length(SP_500$caldt))
                         )

MarketData <- data.frame(
  MarketID = "SP500",
  Date = format(SP_500$caldt, "%d.%m.%Y"),
  ClosingPrice = SP_500$spindx
)

# write.csv(MarketData, file = "03_MarketData.csv", row.names = FALSE)
write.table(MarketData, file = "03_MarketData.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
            )

################## Run Event Study API  ##################

# Setup API Connection
estSetup <- EventStudyAPI$new("http://api.eventstudytools.com") # apiUrl
estSetup$authentication(eventstudytools.API.key) # apiKey

# Initialize Return Event Study object
returnEstParams <- ARCApplicationInput$new()

# Set return type to log
returnEstParams$setReturnType("simple") # default: log
returnEstParams$setBenchmarkModel("mm") # default: mm
returnEstParams$setNonTradingDays("later") # default: later
returnEstParams$setResultFileType("csv") # default: csv
returnEstParams$setTestStatistics(c("art", "cart", "aaraptlz", "caaraptlz", "aargrankt", "caargrankt")) # default: all tests

# Perform Event Study
estResults <- estSetup$performEventStudy(
                  estParams = returnEstParams,
                  dataFiles = c("request_file" = "01_RequestFile.csv",
                                "firm_data"    = "02_firmData.csv",
                                "market_data"  = "03_MarketData.csv"),
                  destDir = dirname(rstudioapi::getSourceEditorContext()$path),
                  downloadFiles = T, # download result files
                  checkFiles    = F) # check input files

test <- subset(Compustat_HQ, f.id %in% c(unique(Compustat_HQ$f.id)))
length(unique(test$f.id))

summary(test$f.duration)
summary(Compustat_HQ$f.duration)
quantile(test$f.duration, probs = seq(0, 1, by = 0.1))
quantile(Compustat_HQ$f.duration, probs = seq(0.1, 0.9, by = 0.1))

##########################################################
################## Customer Event Study ##################
##########################################################

# Get unique values from customer column
unique_customers <- unique(Compustat_HQ$c.gvkey)

# Write the unique values to a plain text file
writeLines(as.character(unique_customers), "unique_customers.txt")

# Customers
Stock_Prices <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230913_Affected Customers Stock Prices.csv"), 
                         header = T,
                         sep = ",")

Stock_Prices$datadate <- as.Date(Stock_Prices$datadate, tryFormats = c("%Y-%m-%d"))

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(Stock_Prices$GVKEY, Stock_Prices$datadate, sep = ", "))

# Bind the new row to the original merged data frame
Stock_Prices <- cbind(Stock_Prices, remove.duplicates)

# Sort the dataframe by remove.duplicates and LINKTYPE
Stock_Prices <- Stock_Prices %>%
  arrange(remove.duplicates, LINKTYPE)

# Keep the first row of each remove.duplicates
Stock_Prices <- Stock_Prices %>%
  distinct(remove.duplicates, .keep_all = TRUE)

# Check if there's still duplicates in remove.duplicates
if(any(duplicated(Stock_Prices$remove.duplicates))) {
  # If there are, keep the one with LINKTYPE == "LC"
  Stock_Prices <- Stock_Prices %>%
    group_by(remove.duplicates) %>%
    filter(LINKTYPE == "LC" | n() == 1) %>%
    ungroup()
}

##### firmData #####

firmData <- data.frame(
  FirmID = rep(NA, length(Stock_Prices$GVKEY)),
  Date = rep(NA, length(Stock_Prices$GVKEY)),
  ClosingPrice = rep(NA, length(Stock_Prices$GVKEY))
)

firmData <- data.frame(
  FirmID = Stock_Prices$conm,
  Date = format(Stock_Prices$datadate, "%d.%m.%Y"),
  ClosingPrice = Stock_Prices$prccd
)

# Convert customer names to lower case
firmData$FirmID <- stri_trans_tolower(firmData$FirmID, locale = "en_US.UTF-8")

# Exlcude dates with NA for Closing Price
firmData <- firmData[!is.na(firmData$ClosingPrice), ]

write.table(firmData, file = "02_firmData.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
)

##### RequestFile #####

# Create an empty data frame with predefined column names
RequestFile <- data.frame(
  EventID = rep(NA, length(Compustat_HQ$f.eventid)),
  FirmID = rep(NA, length(Compustat_HQ$f.id)),
  MarketID = rep(NA, length(Compustat_HQ$f.id)),
  EventDate = rep(NA, length(Compustat_HQ$f.id)),
  GroupingVariable = rep(NA, length(Compustat_HQ$f.id)),
  StartEventWindow = rep(NA, length(Compustat_HQ$f.id)),
  EndEventWindow = rep(NA, length(Compustat_HQ$f.id)),
  EndofEstimationWindow = rep(NA, length(Compustat_HQ$f.id)),
  EstimationWindowLength = rep(NA, length(Compustat_HQ$f.id)),
  stringsAsFactors = FALSE
)

RequestFile <- data.frame(
  EventID = Compustat_HQ$f.eventid,
  FirmID = Compustat_HQ$c.name,
  MarketID = "SP500",
  EventDate = format(Compustat_HQ$f.began, "%d.%m.%Y"),
  GroupingVariable = "None",
# --------------------> SWITCH: ADJUST EVENT WINDOW LENGTH AS DESIRED #####
  StartEventWindow = -9,
  EndEventWindow = 9,
  EndofEstimationWindow = -10,
  EstimationWindowLength = 120
)

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(RequestFile$FirmID, RequestFile$EventDate, sep = ", "))

# Bind the new row to the original merged data frame
RequestFile <- cbind(RequestFile, remove.duplicates)

RequestFile <- RequestFile[!duplicated(RequestFile$remove.duplicates), ]

RequestFile <- subset(RequestFile, select = -remove.duplicates)

length(unique(RequestFile$FirmID))
length(unique(firmData$FirmID))

write.table(RequestFile, file = "01_RequestFile.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
)

##### MarketData #####

SP_500 <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230906_SP500.csv"), 
                   header = T,
                   sep = ",")

SP_500$caldt <- as.Date(SP_500$caldt, tryFormats = c("%Y-%m-%d"))

MarketData <- data.frame(MarketID = rep(NA, length(SP_500$caldt)),
                         Date = rep(NA, length(SP_500$caldt)),
                         ClosingPrice = rep(NA, length(SP_500$caldt))
)

MarketData <- data.frame(
  MarketID = "SP500",
  Date = format(SP_500$caldt, "%d.%m.%Y"),
  ClosingPrice = SP_500$spindx
)

# write.csv(MarketData, file = "03_MarketData.csv", row.names = FALSE)
write.table(MarketData, file = "03_MarketData.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
)

################## Run Event Study API  ##################

# Setup API Connection
estSetup <- EventStudyAPI$new("http://api.eventstudytools.com") # apiUrl
estSetup$authentication(eventstudytools.API.key) # apiKey

# Initialize Return Event Study object
returnEstParams <- ARCApplicationInput$new()

# Set return type to log
returnEstParams$setReturnType("simple") # default: log
returnEstParams$setBenchmarkModel("mm") # default: mm
returnEstParams$setNonTradingDays("later") # default: later
returnEstParams$setResultFileType("csv") # default: csv
returnEstParams$setTestStatistics(c("art", "cart", "aaraptlz", "caaraptlz", "aargrankt", "caargrankt")) # default: all tests

# Perform Event Study
estResults <- estSetup$performEventStudy(
  estParams = returnEstParams,
  dataFiles = c("request_file" = "01_RequestFile.csv",
                "firm_data"    = "02_firmData.csv",
                "market_data"  = "03_MarketData.csv"),
  destDir = dirname(rstudioapi::getSourceEditorContext()$path),
  downloadFiles = T, # download result files
  checkFiles    = F) # check input files

test <- subset(Compustat_HQ, f.id %in% c(unique(Compustat_HQ$f.id)))
length(unique(test$f.id))

summary(test$f.duration)
summary(Compustat_HQ$f.duration)
quantile(test$f.duration, probs = seq(0, 1, by = 0.1))
quantile(Compustat_HQ$f.duration, probs = seq(0.1, 0.9, by = 0.1))

##########################################################
################## S&P Capital IQ Pro ####################
##########################################################

Compustat_HQ <- Compustat_HQ_NegAffSuppliersBackup

SPCapIQPro <- read_excel(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230914_SPCapIQPro.xlsx"))

length(unique(SPCapIQPro$s.gvkey))

SPCapIQPro$ciq.customername <- stri_trans_tolower(SPCapIQPro$ciq.customername, locale = "en_US.UTF-8")

CIQ_Identifier <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230724_CIQ_Identifier.csv"), 
                         header = T,
                         sep = ",")

CIQ_Identifier$companyname <- stri_trans_tolower(CIQ_Identifier$companyname, locale = "en_US.UTF-8")
CIQ_Identifier <- CIQ_Identifier[, c("companyname",
                                     "gvkey",
                                     "companyid"
)]

CIQ_Identifier <- CIQ_Identifier %>% 
  rename(ciq.companyname = companyname,
         ciq.gvkey = gvkey,
         ciq.companyid = companyid
  )


SPCapIQPro <- merge(SPCapIQPro, CIQ_Identifier, by.x = c("ciq.customername"), by.y = c("ciq.companyname"), all.x = TRUE)

SPCapIQPro <- subset(SPCapIQPro, !is.na(ciq.gvkey))

# Load customer locations data
All_HQ_addresses <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230614_All HQ addresses.csv"), 
                             header = T,
                             sep = ",")

# We reformat the date column
All_HQ_addresses$datadate <- as.Date(All_HQ_addresses$datadate, tryFormats = c("%Y-%m-%d"))
All_HQ_addresses$datadate.year <- year(All_HQ_addresses$datadate)

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(All_HQ_addresses$gvkey, All_HQ_addresses$datadate.year, sep = ", "))

# Bind the new row to the original merged data frame
All_HQ_addresses <- cbind(All_HQ_addresses, remove.duplicates)

All_HQ_addresses <- All_HQ_addresses[!duplicated(All_HQ_addresses$remove.duplicates), ]

All_HQ_addresses <- All_HQ_addresses[, c("datadate.year",
                                         "gvkey", 
                                         "add1", 
                                         "add2", 
                                         "add3", 
                                         "add4", 
                                         "addzip",
                                         "city",
                                         "idbflag",
                                         "loc",
                                         "state"
)]

SPCapIQPro$s.srcdate.year <- year(SPCapIQPro$s.srcdate)

# Create a new row with the concatenated values
SPCapIQPro <- merge(SPCapIQPro, All_HQ_addresses, by.x = c("s.srcdate.year", "ciq.gvkey"), by.y = c("datadate.year", "gvkey"), all.x = TRUE)

# Remove observations before 1985
SPCapIQPro <- subset(SPCapIQPro, s.srcdate >= "1985-01-01")

# Remove observations after 2022
SPCapIQPro <- subset(SPCapIQPro, s.srcdate <= "2022-12-31")

# Rename all columns starting with s. for supplier information and c. for customer information
SPCapIQPro <- SPCapIQPro %>% 
  rename(c.add1 = add1,
         c.add2 = add2,
         c.add3 = add3,
         c.add4 = add4,
         c.addzip = addzip,
         c.city = city,
         c.idbflag = idbflag,
         c.loc = loc,
         c.state = state
  )

# Remove non-US customers
SPCapIQPro <- subset(SPCapIQPro, c.loc == "USA")

# Remove non-domestic customers
SPCapIQPro <- subset(SPCapIQPro, c.idbflag == "D")

# write.csv(SPCapIQPro, file = "/Users/timomeier/Desktop/Studium/MBF/MA/Data/SPCapIQPro.csv")

SPCapIQPro <- subset(SPCapIQPro, (is.na(ciq.startdate) | f.year.x >= ciq.startdate))

SPCapIQPro <- subset(SPCapIQPro, (is.na(ciq.enddate) | f.year.x <= ciq.enddate))

SPCapIQPro <- subset(SPCapIQPro, s.gvkey != ciq.gvkey)

# Create a new row with the concatenated values
c.address <- data.frame(c.address = paste(SPCapIQPro$c.add1,
                                          SPCapIQPro$c.city,
                                          SPCapIQPro$c.state,
                                          SPCapIQPro$c.addzip,
                                          SPCapIQPro$c.loc,
                                          sep = ", "
)
)

# Bind the new row to the original merged data frame
SPCapIQPro <- cbind(SPCapIQPro, c.address)

##### CapIQ Customer addresses #####

# Add latitude and longitude columns
SPCapIQPro$c.latitude <- NA
SPCapIQPro$c.longitude <- NA

# Initialize a data frame to store failed addresses
Failed_Geocoding_Customers <- data.frame(
  index = integer(),
  address = character(),
  stringsAsFactors = FALSE
)

# Define the geocode_address function
geocode_address <- function(address) {
  base_url <- "http://dev.virtualearth.net/REST/v1/Locations"
  query_params <- list(
    query = address,
    key = bing.maps.API.key
  )
  response <- GET(url = base_url, query = query_params)
  if (http_type(response) == "application/json") {
    result <- content(response, as = "parsed")
    if (result$statusDescription == "OK" && result$resourceSets[[1]]$estimatedTotal > 0) {
      coordinates <- result$resourceSets[[1]]$resources[[1]]$point$coordinates
      return(coordinates)
    }
  }
  return(NULL)
}

# Loop through the addresses and geocode them
for (i in 1:nrow(SPCapIQPro)) {
  address <- SPCapIQPro$c.address[i]
  
  # Print out the current index and address
  cat("Processing customer address", i, ":", address, "\n")
  
  coordinates <- geocode_address(address)
  if (!is.null(coordinates)) {
    latitude <- coordinates[1]
    longitude <- coordinates[2]
    SPCapIQPro$c.latitude[i] <- latitude
    SPCapIQPro$c.longitude[i] <- longitude
  } else {
    SPCapIQPro$c.latitude[i] <- NA
    SPCapIQPro$c.longitude[i] <- NA
    cat("Geocoding failed for the address:", address, "\n")
    
    # Add the failed address to the dataframe
    Failed_Geocoding_Customers <- rbind(Failed_Geocoding_Customers, data.frame(index = i, address = address))
  }
}

SPCapIQPro$c.longitude <- unlist(SPCapIQPro$c.longitude)
SPCapIQPro$c.latitude <- unlist(SPCapIQPro$c.latitude)

# Here I decided to save a legacy copy
# write.csv(SPCapIQPro, file = file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230914_SPCapIQPro.csv"))

SPCapIQPro <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230914_SPCapIQPro.csv"), 
                         header = T,
                         sep = ",")

length(unique(SPCapIQPro$s.name))

SPCapIQPro_Customers <- SPCapIQPro

# Step 1: Convert `SPCapIQPro_Customers` to a spatial points dataframe
SPCapIQPro_Customers <- st_as_sf(SPCapIQPro_Customers, coords = c("c.longitude", "c.latitude"), crs = 4326)

# Set the CRS for Compustat_HQ
st_crs(SPCapIQPro_Customers) <- 4326

# Now, transform Compustat_HQ to match the CRS of floods_US if they are not the same
if (st_crs(SPCapIQPro_Customers) != st_crs(floods_US)) {
  SPCapIQPro_Customers <- st_transform(Compustat_HQ, st_crs(floods_US))
}

# Find intersections
intersections <- st_intersects(SPCapIQPro_Customers, floods_US)

# Perform a spatial join between `Compustat_HQ` and `floods_US` datasets
SPCapIQPro_Customers <- st_join(SPCapIQPro_Customers, floods_US, join = st_intersects)

SPCapIQPro_Customers_FloodsBackup <- SPCapIQPro_Customers

SPCapIQPro_Customers <- subset(SPCapIQPro_Customers, f.year.x == f.year)

SPCapIQPro_Customers_Merge <- data.frame(
  ciq.gvkey=rep(NA, length(SPCapIQPro_Customers$f.id.y)),
  s.srcdate=rep(NA, length(SPCapIQPro_Customers$f.id.y)), 
  f.id=rep(NA, length(SPCapIQPro_Customers$f.id.y)),
  f.eventid=rep(NA, length(SPCapIQPro_Customers$f.id.y)),
  stringsAsFactors = FALSE
)

SPCapIQPro_Customers_Merge$ciq.gvkey <- SPCapIQPro_Customers$ciq.gvkey
SPCapIQPro_Customers_Merge$s.srcdate <- SPCapIQPro_Customers$s.srcdate
SPCapIQPro_Customers_Merge$f.id <- SPCapIQPro_Customers$f.id.y
SPCapIQPro_Customers_Merge$f.eventid <- SPCapIQPro_Customers$f.eventid

SPCapIQPro <- merge(SPCapIQPro, SPCapIQPro_Customers_Merge, by.x = c("s.srcdate", "ciq.gvkey", "f.eventid"), by.y = c("s.srcdate", "ciq.gvkey", "f.eventid"), all.x = TRUE
                      #, all.y = TRUE
)

#view(SPCapIQPro[, c("s.name","f.id.x","ciq.customername", "f.id.y")])

SPCapIQPro <- subset(SPCapIQPro, (is.na(f.id.x) | is.na(f.id.y)) | f.id.x != f.id.y)

SPCapIQPro <- subset(SPCapIQPro, select = -f.id.y)

SPCapIQPro <- SPCapIQPro %>% 
  rename(f.id = f.id.x
  )

# Create a new row with the concatenated values of all columns
remove.duplicates <- data.frame(remove.duplicates = apply(SPCapIQPro, 1, paste, collapse = ", "))

# Bind the new row to the original merged data frame
SPCapIQPro <- cbind(SPCapIQPro, remove.duplicates)

# Remove duplicated rows based on the 'remove.duplicates' column
SPCapIQPro <- SPCapIQPro[!duplicated(SPCapIQPro$remove.duplicates), ]

# Remove the 'remove.duplicates' column
SPCapIQPro <- subset(SPCapIQPro, select = -remove.duplicates)

SPCapIQPro$f.eventid.2 <- 1:length(SPCapIQPro$s.gvkey)

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(SPCapIQPro$ciq.customername, SPCapIQPro$f.id, sep = ", "))

# Bind the new row to the original merged data frame
SPCapIQPro <- cbind(SPCapIQPro, remove.duplicates)

##### CapIQ Customer data cleansing #######

SPCapIQPro_Customers <- SPCapIQPro_Customers_FloodsBackup
SPCapIQPro_Customers <- subset(SPCapIQPro_Customers, ciq.gvkey %in% unique(SPCapIQPro$ciq.gvkey))

# Select only the required columns and filter out duplicates
unique_data_supplierdates <- unique(SPCapIQPro[, c("ciq.customername", "f.id", "f.began")])
unique_data_customerdates <- unique(SPCapIQPro_Customers[, c("ciq.customername", "f.id.y", "f.began.y")])

unique_data_customerdates <- unique_data_customerdates %>% 
  rename(f.id = f.id.y,
         f.began = f.began.y
  )

unique_data_customerdates$geometry <- NULL

# be careful about ciq. check which f.id to match etc.

unique_data_supplierdates$eventdatesofinterest <- "Yes"
unique_data_customerdates$eventdatesofinterest <- "No"

# Convert the 'f.began' column to a Date type
unique_data_customerdates$f.began <- as.Date(unique_data_customerdates$f.began, format = "%Y-%m-%d")
unique_data_supplierdates$f.began <- as.Date(unique_data_supplierdates$f.began, format = "%Y-%m-%d")

unique_data <- rbind(unique_data_supplierdates, unique_data_customerdates)

# Convert the 'f.began' column to a Date type
unique_data$f.began <- as.Date(unique_data$f.began, format = "%Y-%m-%d")

# Calculate the difference in days for the same s.name
unique_data <- unique_data[order(unique_data$ciq.customername, unique_data$f.began), ]
unique_data$days_diff <- ave(as.numeric(unique_data$f.began), unique_data$ciq.customername, FUN = function(x) c(NA, diff(x)))

# Find s.name and f.id combinations where the difference is less than 180 days
to_exclude <- unique_data[!is.na(unique_data$days_diff) & unique_data$days_diff < 181 | unique_data$eventdatesofinterest == "No", c("ciq.customername", "f.id")]

table(unique_data$eventdatesofinterest)

# Exclude combinations of s.name and f.id in to_exclude from SPCapIQPro
SPCapIQPro <- SPCapIQPro[!paste(SPCapIQPro$ciq.customername, SPCapIQPro$f.id) %in% paste(to_exclude$ciq.customername, to_exclude$f.id), ]

# create a new column f.duration by subtracting the dates
SPCapIQPro$f.began <- as.Date(SPCapIQPro$f.began)
SPCapIQPro$f.ended <- as.Date(SPCapIQPro$f.ended)
SPCapIQPro$f.duration <- as.numeric(SPCapIQPro$f.ended - SPCapIQPro$f.began)

SPCapIQPro <- merge(SPCapIQPro, Link.gvkey, by.x = c("ciq.gvkey"), by.y = c("gvkey"), all.x = TRUE)

SPCapIQPro <- SPCapIQPro %>% 
  rename(ciq.conm = conm,
         ciq.sic = sic,
  )

SPCapIQPro <- SPCapIQPro[!is.na(SPCapIQPro$ciq.conm), ]

length(unique(SPCapIQPro$f.id))
length(unique(SPCapIQPro$s.gvkey))
length(unique(SPCapIQPro$ciq.gvkey))

test <- subset(SPCapIQPro, f.id %in% c(unique(SPCapIQPro$f.id)))
summary(test$f.duration)

Descriptive_SPCapIQPro_Customers <- subset(SPCapIQPro, f.id %in% c(unique(SPCapIQPro$f.id)))
summary(Descriptive_SPCapIQPro_Customers$f.duration)

floods_descriptive <- floods_US[, c("f.id", "f.dead", "f.displaced", "f.severity", "f.area")]

Descriptive_SPCapIQPro_Customers <- merge(Descriptive_SPCapIQPro_Customers, floods_descriptive, by.x = c("f.id"), by.y = c("f.id"), all.x = TRUE)

Descriptive_SPCapIQPro_Customers <- Descriptive_SPCapIQPro_Customers[, c("f.duration", "f.dead", "f.displaced", "f.severity", "f.area")]
Descriptive_SPCapIQPro_Customers$geometry <- NULL

stargazer(Descriptive_SPCapIQPro_Customers,
          type = "latex",
          summary = T,
          summary.stat = c("N", "mean", "min", "p25", "median", "p75", "max", "sd")
)

# Filter out false observations SP CAP IQ PRO CUSTOMERS
SPCapIQPro <- subset(SPCapIQPro, !f.eventid.2 %in% c(
  # Round 1: shorter / extended / moved too far
  15,65,115,146,190,
  # Round 2: shorter / extended / moved too far
  14,191,192,
  # Analysis performed with errors: 
  16,32,74,93,121,138,176,186,187,223
))

length(unique(SPCapIQPro$f.id))
length(unique(SPCapIQPro$s.gvkey))
length(unique(SPCapIQPro$ciq.gvkey))

# Get unique values from customer column
unique_customers <- unique(SPCapIQPro$ciq.gvkey)

# Write the unique values to a plain text file
writeLines(as.character(unique_customers), "unique_customers.txt")

# Customers
SPCapIQPro_Stock_Prices <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230914_SPCapIQPro_Affected Customers Stock Prices.csv"), 
                         header = T,
                         sep = ",")

SPCapIQPro_Stock_Prices$datadate <- as.Date(SPCapIQPro_Stock_Prices$datadate, tryFormats = c("%Y-%m-%d"))

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(SPCapIQPro_Stock_Prices$GVKEY, SPCapIQPro_Stock_Prices$datadate, sep = ", "))

# Bind the new row to the original merged data frame
SPCapIQPro_Stock_Prices <- cbind(SPCapIQPro_Stock_Prices, remove.duplicates)

# Sort the dataframe by remove.duplicates and LINKTYPE
SPCapIQPro_Stock_Prices <- SPCapIQPro_Stock_Prices %>%
  arrange(remove.duplicates, LINKTYPE)

# Keep the first row of each remove.duplicates
SPCapIQPro_Stock_Prices <- SPCapIQPro_Stock_Prices %>%
  distinct(remove.duplicates, .keep_all = TRUE)

# Check if there's still duplicates in remove.duplicates
if(any(duplicated(SPCapIQPro_Stock_Prices$remove.duplicates))) {
  # If there are, keep the one with LINKTYPE == "LC"
  SPCapIQPro_Stock_Prices <- SPCapIQPro_Stock_Prices %>%
    group_by(remove.duplicates) %>%
    filter(LINKTYPE == "LC" | n() == 1) %>%
    ungroup()
}

##### firmData #####

firmData <- data.frame(
  FirmID = rep(NA, length(SPCapIQPro_Stock_Prices$GVKEY)),
  Date = rep(NA, length(SPCapIQPro_Stock_Prices$GVKEY)),
  ClosingPrice = rep(NA, length(SPCapIQPro_Stock_Prices$GVKEY))
)

firmData <- data.frame(
  FirmID = SPCapIQPro_Stock_Prices$conm,
  Date = format(SPCapIQPro_Stock_Prices$datadate, "%d.%m.%Y"),
  ClosingPrice = SPCapIQPro_Stock_Prices$prccd
)

# Convert customer names to lower case
firmData$FirmID <- stri_trans_tolower(firmData$FirmID, locale = "en_US.UTF-8")

# Exlcude dates with NA for Closing Price
firmData <- firmData[!is.na(firmData$ClosingPrice), ]

write.table(firmData, file = "02_firmData.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
)

##### RequestFile #####

# Create an empty data frame with predefined column names
RequestFile <- data.frame(
  EventID = rep(NA, length(SPCapIQPro$f.eventid.2)),
  FirmID = rep(NA, length(SPCapIQPro$f.id)),
  MarketID = rep(NA, length(SPCapIQPro$f.id)),
  EventDate = rep(NA, length(SPCapIQPro$f.id)),
  GroupingVariable = rep(NA, length(SPCapIQPro$f.id)),
  StartEventWindow = rep(NA, length(SPCapIQPro$f.id)),
  EndEventWindow = rep(NA, length(SPCapIQPro$f.id)),
  EndofEstimationWindow = rep(NA, length(SPCapIQPro$f.id)),
  EstimationWindowLength = rep(NA, length(SPCapIQPro$f.id)),
  stringsAsFactors = FALSE
)

RequestFile <- data.frame(
  EventID = SPCapIQPro$f.eventid.2,
  FirmID = SPCapIQPro$ciq.conm,
  MarketID = "SP500",
  EventDate = format(SPCapIQPro$f.began, "%d.%m.%Y"),
  GroupingVariable = "None",
# --------------------> SWITCH: ADJUST EVENT WINDOW LENGTH AS DESIRED #####
  StartEventWindow = -12,
  EndEventWindow = 12,
  EndofEstimationWindow = -13,
  EstimationWindowLength = 120
)

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(RequestFile$FirmID, RequestFile$EventDate, sep = ", "))

# Bind the new row to the original merged data frame
RequestFile <- cbind(RequestFile, remove.duplicates)

RequestFile <- RequestFile[!duplicated(RequestFile$remove.duplicates), ]

RequestFile <- subset(RequestFile, select = -remove.duplicates)

length(unique(firmData$FirmID))

write.table(RequestFile, file = "01_RequestFile.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
)

##### MarketData #####

SP_500 <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230906_SP500.csv"), 
                   header = T,
                   sep = ",")

SP_500$caldt <- as.Date(SP_500$caldt, tryFormats = c("%Y-%m-%d"))

MarketData <- data.frame(MarketID = rep(NA, length(SP_500$caldt)),
                         Date = rep(NA, length(SP_500$caldt)),
                         ClosingPrice = rep(NA, length(SP_500$caldt))
)

MarketData <- data.frame(
  MarketID = "SP500",
  Date = format(SP_500$caldt, "%d.%m.%Y"),
  ClosingPrice = SP_500$spindx
)

write.table(MarketData, file = "03_MarketData.csv", sep = ";", row.names = FALSE, col.names = FALSE, quote = FALSE #, fileEncoding="ASCII"
)

################## Run Event Study API  ##################

# Setup API Connection
estSetup <- EventStudyAPI$new("http://api.eventstudytools.com") # apiUrl
estSetup$authentication(eventstudytools.API.key) # apiKey

# Initialize Return Event Study object
returnEstParams <- ARCApplicationInput$new()

# Set return type to log
returnEstParams$setReturnType("simple") # default: log
returnEstParams$setBenchmarkModel("mm") # default: mm
returnEstParams$setNonTradingDays("later") # default: later
returnEstParams$setResultFileType("csv") # default: csv
returnEstParams$setTestStatistics(c("art", "cart", "aaraptlz", "caaraptlz", "aargrankt", "caargrankt")) # default: all tests

# Perform Event Study
estResults <- estSetup$performEventStudy(
  estParams = returnEstParams,
  dataFiles = c("request_file" = "01_RequestFile.csv",
                "firm_data"    = "02_firmData.csv",
                "market_data"  = "03_MarketData.csv"),
  destDir = dirname(rstudioapi::getSourceEditorContext()$path),
  downloadFiles = T, # download result files
  checkFiles    = F) # check input files

test <- subset(SPCapIQPro, f.id %in% c(unique(SPCapIQPro$f.id)))
summary(test$f.duration)

view(subset(floods, ID %in% c(unique(SPCapIQPro$f.id))))
view(subset(SPCapIQPro, f.id == 4523))
view(subset(SPCapIQPro, f.id == 2895))

summary(test$f.duration)
summary(SPCapIQPro$f.duration)
quantile(test$f.duration, probs = seq(0, 1, by = 0.1))
quantile(SPCapIQPro$f.duration, probs = seq(0.1, 0.9, by = 0.1))

view(SPCapIQPro[, c("ciq.conm","ciq.customername","ciq.gvkey")])

##########################################################
################## Multivariate Analyses #################
##########################################################

#######################
##### Preparation #####
#######################

# --------------------> SWITCH: ADJUST EVENT WINDOW LENGTH AS DESIRED #####
date_range <- "-12,12"

CARresults <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Event Study Results/SPCapIQPro Sample/", date_range, "/car_results.csv"), 
                       header = T,
                       sep = ";")

CARresults <- CARresults[, c("Event.ID", "CAR.value")]

AnalysisReport <- read.csv(paste0(dirname(rstudioapi::getSourceEditorContext()$path),"/Event Study Results/SPCapIQPro Sample/", date_range, "/analysis_report.csv"), 
                           header = T,
                           sep = ";")

AnalysisReport <- AnalysisReport[, c("Event.ID", "Beta")]

CARresults <- merge(CARresults, AnalysisReport, by.x = c("Event.ID"), by.y = c("Event.ID"), all.x = TRUE)

SPCapIQPro_CAR <- SPCapIQPro[, c("ciq.gvkey", "s.gvkey", "f.eventid.2", "s.srcdate", "s.srcdate.year", "ciq.customername", "ciq.sic", "f.year.x", "f.id", "f.began", "f.ended", "f.duration")]

floods_CAR <- floods_US[, c("f.id", "f.dead", "f.displaced", "f.severity")]

SPCapIQPro_CAR <- merge(SPCapIQPro_CAR, floods_CAR, by.x = c("f.id"), by.y = c("f.id"), all.x = TRUE)

CARresults <- merge(CARresults, SPCapIQPro_CAR, by.x = c("Event.ID"), by.y = c("f.eventid.2"), all.x = TRUE)

##### Compute customer number of previous floods #####

PrevFloodsCustomers <- SPCapIQPro_Customers_FloodsBackup

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(PrevFloodsCustomers$ciq.gvkey, PrevFloodsCustomers$f.id.y, sep = ", "))

# Bind the new row to the original merged data frame
PrevFloodsCustomers <- cbind(PrevFloodsCustomers, remove.duplicates)

PrevFloodsCustomers <- PrevFloodsCustomers[!duplicated(PrevFloodsCustomers$remove.duplicates), ]

# Order by ciq.gvkey and then by f.id.y, group by ciq.gvkey, and add a new column flood_number
PrevFloodsCustomers <- PrevFloodsCustomers %>%
  arrange(`ciq.gvkey`, `f.began.y`) %>%
  group_by(`ciq.gvkey`) %>%
  mutate(f.prev.customer = row_number()) %>%
  ungroup()

PrevFloodsCustomers <- PrevFloodsCustomers[, c("ciq.gvkey", "f.began.y", "f.prev.customer")]

PrevFloodsCustomers$geometry <- NULL

# For each unique customer in CARresults
CARresults <- CARresults %>%
  rowwise() %>%
  mutate(
    f.prev.customer = {
      # Find the rows in PrevFloodsCustomers that match the ciq.gvkey and the date condition
      matched_floods <- PrevFloodsCustomers[PrevFloodsCustomers$ciq.gvkey == ciq.gvkey & 
                                              PrevFloodsCustomers$f.began.y <= f.began,]
      
      # If no match is found, set result as 0
      if(nrow(matched_floods) == 0) {
        result <- 0
      } else {
        # Find the row with the closest date
        closest_row <- which.min(abs(difftime(matched_floods$f.began.y, f.began, units = "days")))
        
        # Ensure that there's a valid closest_row
        if (length(closest_row) == 0) {
          result <- 0
        } else {
          # Extract the f.prev.customer value
          result <- matched_floods$f.prev.customer[closest_row]
        }
      }
      result
    }
  ) %>%
  ungroup()

##### Compute supplier number of previous floods #####

PrevFloodsSuppliers <- Compustat_HQ_Suppliers_FloodsBackup

# Create a new row with the concatenated values
remove.duplicates <- data.frame(remove.duplicates = paste(PrevFloodsSuppliers$s.gvkey, PrevFloodsSuppliers$f.id, sep = ", "))

# Bind the new row to the original merged data frame
PrevFloodsSuppliers <- cbind(PrevFloodsSuppliers, remove.duplicates)

PrevFloodsSuppliers <- PrevFloodsSuppliers[!duplicated(PrevFloodsSuppliers$remove.duplicates), ]

PrevFloodsSuppliers <- PrevFloodsSuppliers %>%
  arrange(`s.gvkey`, `f.began`) %>%
  group_by(`s.gvkey`) %>%
  mutate(f.prev.supplier = row_number()) %>%
  ungroup()

PrevFloodsSuppliers <- PrevFloodsSuppliers[, c("s.gvkey", "f.began", "f.prev.supplier")]

PrevFloodsSuppliers$geometry <- NULL

# For each unique customer in CARresults
CARresults <- CARresults %>%
  rowwise() %>%
  mutate(
    f.prev.supplier = {
      # Find the rows in PrevFloodsSuppliers that match the ciq.gvkey and the date condition
      matched_floods <- PrevFloodsSuppliers[PrevFloodsSuppliers$s.gvkey == s.gvkey & 
                                              PrevFloodsSuppliers$f.began <= f.began,]
      
      # If no match is found, set result as 0
      if(nrow(matched_floods) == 0) {
        result <- 0
      } else {
        # Find the row with the closest date
        closest_row <- which.min(abs(difftime(matched_floods$f.began, f.began, units = "days")))
        
        # Ensure that there's a valid closest_row
        if (length(closest_row) == 0) {
          result <- 0
        } else {
          # Extract the f.prev.supplier value
          result <- matched_floods$f.prev.supplier[closest_row]
        }
      }
      result
    }
  ) %>%
  ungroup()

CARresults$geometry <- NULL

CARresults_Backup <- CARresults

############################################################
##### Analyses: Run the desired one ########################
############################################################

##### Compustat Daily Updates - Fundamentals Annual ########

CARresults <- CARresults_Backup

Fundamentals_Annual <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/231005_Fundamentals Annual.csv"), 
                header = T,
                sep = ",")

Fundamentals_Annual$year <- year(Fundamentals_Annual$datadate)

Fundamentals_Annual$tobinsq <- (Fundamentals_Annual$at + Fundamentals_Annual$csho * Fundamentals_Annual$prcc_f - (Fundamentals_Annual$at - (Fundamentals_Annual$lt + Fundamentals_Annual$pstk) + Fundamentals_Annual$txditc)) / Fundamentals_Annual$at

Fundamentals_Annual$tangibility <- Fundamentals_Annual$ppent / Fundamentals_Annual$at

Fundamentals_Annual$leverage <- (Fundamentals_Annual$dlc + Fundamentals_Annual$dltt) / (Fundamentals_Annual$dlc + Fundamentals_Annual$dltt + Fundamentals_Annual$csho * Fundamentals_Annual$prcc_f)

Fundamentals_Annual$ltdebtmaturing <- Fundamentals_Annual$dd1 / (Fundamentals_Annual$dd1 + Fundamentals_Annual$dltt)

Fundamentals_Annual$ln.at <- log(Fundamentals_Annual$at)

Fundamentals_Annual$chetoat <- Fundamentals_Annual$che / Fundamentals_Annual$at

Fundamentals_Annual$ln.emptoln.at <- log(Fundamentals_Annual$emp) / log(Fundamentals_Annual$at)

CARresults <- merge(CARresults, Fundamentals_Annual, by.x = c("ciq.gvkey", "f.year.x"), by.y = c("gvkey", "year"), all.x = TRUE)

# Replace observations which do not pass the diagnostic tests, adjust as needed per model
# CARresults <- CARresults[-50,]
# CARresults <- CARresults[-4,]

cols_to_check <- c(
"f.duration",
"f.severity",
"f.prev.customer",
"f.prev.supplier",
"chetoat",
"ln.at",
"tangibility",
"leverage",
"ltdebtmaturing",
"tobinsq",
"ln.emptoln.at",
"Beta"
)

CARresults <- CARresults[complete.cases(CARresults[cols_to_check]), ]

lm1 <- lm(data = CARresults, CAR.value ~ 
         f.duration
       + f.severity # Cancel in parsimonious
       + f.prev.customer
       + f.prev.supplier
       + chetoat # Cancel in parsimonious
       + ln.at
       + tangibility # Cancel in parsimonious
       + leverage # Cancel in parsimonious
       + ltdebtmaturing # Cancel in parsimonious
       + tobinsq
       + ln.emptoln.at # Cancel in parsimonious
       + Beta # Cancel in parsimonious
)

vif(lm1)
plot(lm1, 4)

# --------------------> SWITCH: ADJUST NAME TO CHOSEN EVENT WINDOW, STORE VALUES, AND RERUN THE CODE IN THIS SECTION FOR OTHER WINDOWS #####
# For example, if you ran a (-12;12) event study, store lm1 in an object called "lm1.12" before repeating the process fo other windows
lm1.12 <- lm1
lm1.12_robust <- coeftest(lm1, vcov. = NeweyWest(lm1))

stargazer(lm1.3_robust, lm1.6_robust, lm1.9_robust, lm1.12_robust,
       type = "latex", 
       report = "vc*t",
       column.labels = c("(-3,3)", "(-6,6)", "(-9,9)", "(-12,12)"),
       summary = TRUE
       )

stargazer(lm1.3, lm1.6, lm1.9, lm1.12, 
       type = "text",
       report = "vc*t",
       column.labels = c("(-3,3)", "(-6,6)", "(-9,9)", "(-12,12)"),
       summary = TRUE
)

CARresults <- CARresults[, c(
"CAR.value",
"f.duration",
"f.severity",
"f.prev.customer",
"f.prev.supplier",
"chetoat",
"ln.at",
"tangibility",
"leverage",
"ltdebtmaturing",
"tobinsq",
"ln.emptoln.at",
"Beta"
)]

stargazer(CARresults,
       type = "text",
       summary = T,
       summary.stat = c("N", "mean", "p25", "median", "p75", "sd")
)

##### Diagnostics #####

# Variance Inflation Factors
vif(lm1)

# Influential Outliers
plot(lm1, 4)

# Heteroscedasticity
bptest(lm1)

# Autocorrelation
dwtest(lm1)

# Normal distribution of residuals
lillieTest(residuals(lm1))

# Diagnostic Plots
plot(lm1, 1:6)

##### Compustat Daily Updates - Fundamentals Quarterly #####

CARresults <- CARresults_Backup

MV_IVs <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230923_MV Independent Variables.csv"), 
                      header = T,
                      sep = ",")

# Determine the fiscal quarter based on the month
CARresults$f.began.fquarter <- ifelse(month(CARresults$f.began) %in% c(2,3,4), "Q1",
                                     ifelse(month(CARresults$f.began) %in% c(5,6,7), "Q2",
                                            ifelse(month(CARresults$f.began) %in% c(8,9,10), "Q3", "Q4")))

# Adjust the year for fiscal Q4 (Nov, Dec, Jan) to be the next year
adj_year <- year(CARresults$f.began)
adj_year[month(CARresults$f.began) %in% c(11, 12, 1)] <- adj_year[month(CARresults$f.began) %in% c(11, 12, 1)] + 1

# Create the desired format "YYYYQX" for fiscal quarters
CARresults$f.began.fquarter <- paste0(adj_year, CARresults$f.began.fquarter)

# Determine the previous fiscal quarter
CARresults$f.began.prevfquarter <- ifelse(substr(CARresults$f.began.fquarter, 5, 6) == "Q1", 
                                        paste0(as.numeric(substr(CARresults$f.began.fquarter, 1, 4)) - 1, "Q4"),
                                        ifelse(substr(CARresults$f.began.fquarter, 5, 6) == "Q2", 
                                               paste0(substr(CARresults$f.began.fquarter, 1, 4), "Q1"),
                                               ifelse(substr(CARresults$f.began.fquarter, 5, 6) == "Q3", 
                                                      paste0(substr(CARresults$f.began.fquarter, 1, 4), "Q2"),
                                                      ifelse(substr(CARresults$f.began.fquarter, 5, 6) == "Q4",
                                                             paste0(substr(CARresults$f.began.fquarter, 1, 4), "Q3"),
                                                             NA))))

MV_IVs$tobinsq <- (MV_IVs$atq + MV_IVs$cshoq * MV_IVs$prccq - (MV_IVs$atq - (MV_IVs$ltq + MV_IVs$pstkq) + MV_IVs$txditcq)) / MV_IVs$atq

MV_IVs$tangibility <- MV_IVs$ppentq / MV_IVs$atq

MV_IVs$leverage <- (MV_IVs$dlcq + MV_IVs$dlttq) / (MV_IVs$dlcq + MV_IVs$dlttq + MV_IVs$cshoq * MV_IVs$prccq)

MV_IVs$ltdebtmaturing <- MV_IVs$dd1q / (MV_IVs$dd1q + MV_IVs$dlttq)

MV_IVs$ln.atq <- log(MV_IVs$atq)

MV_IVs$cheqtoatq<- MV_IVs$cheq / MV_IVs$atq

CARresults <- merge(CARresults, MV_IVs, by.x = c("ciq.gvkey", "f.began.prevfquarter"), by.y = c("gvkey", "datafqtr"), all.x = TRUE)

cols_to_check <- c(
 "f.duration",
 "f.severity",
 "f.prev.customer",
 "f.prev.supplier",
 "cheqtoatq",
 "ln.atq",
 "tangibility",
 "leverage",
 "ltdebtmaturing",
 "tobinsq",
 "Beta"
)

CARresults <- CARresults[complete.cases(CARresults[cols_to_check]), ]

lm2 <- lm(data = CARresults, CAR.value ~ 
           f.duration
         + f.severity # Cancel in parsimonious
         + f.prev.customer
         + f.prev.supplier # Cancel in parsimonious
         + cheqtoatq # Cancel in parsimonious
         + ln.atq # Cancel in parsimonious
         + tangibility
         + leverage
         + ltdebtmaturing
         + tobinsq
         + Beta # Cancel in parsimonious
)

vif(lm2)
plot(lm2, 4)

# --------------------> SWITCH: ADJUST NAME TO CHOSEN EVENT WINDOW, STORE VALUES, AND RERUN THE CODE IN THIS SECTION FOR OTHER WINDOWS #####
# For example, if you ran a (-12;12) event study, store lm2 in an object called "lm2.12" before repeating the process fo other windows
lm2.12 <- lm2
lm2.12_robust <- coeftest(lm2, vcov. = NeweyWest(lm2))

stargazer(lm2.3_robust, lm2.6_robust, lm2.9_robust, lm2.12_robust,
         type = "latex", 
         report = "vc*t",
         column.labels = c("(-3,3)", "(-6,6)", "(-9,9)", "(-12,12)"),
         summary = TRUE
)

stargazer(lm2.3, lm2.6, lm2.9, lm2.12, 
         type = "latex",
         report = "vc*t",
         column.labels = c("(-3,3)", "(-6,6)", "(-9,9)", "(-12,12)"),
         summary = TRUE
)

CARresults <- CARresults[, c(
 "CAR.value",
 "f.duration",
 "f.severity",
 "f.prev.customer",
 "f.prev.supplier",
 "cheqtoatq",
 "ln.atq",
 "tangibility",
 "leverage",
 "ltdebtmaturing",
 "tobinsq",
 "Beta"
)]

stargazer(CARresults,
         type = "text",
         summary = T,
         summary.stat = c("N", "mean", "p25", "median", "p75", "sd")
)

##### Diagnostics #####

# Variance Inflation Factors
vif(lm2)

# Influential Outliers
plot(lm2, 4)

# Heteroscedasticity
bptest(lm2)

# Autocorrelation
dwtest(lm2)

# Normal distribution of residuals
lillieTest(residuals(lm2))

# Diagnostic Plots
plot(lm2, 1:6)

##### Financial Ratios Firm Level by WRDS (Beta) #####

CARresults <- CARresults_Backup

MV_FinancialRatios <- read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),"Data/230923_SPCapIQ Pro Customers Financial Variables.csv"), 
                               header = T,
                               sep = ",")

MV_FinancialRatios$public_date <- as.Date(MV_FinancialRatios$public_date, tryFormats = c("%Y-%m-%d"))

MV_FinancialRatios$public_date_month <- format(as.Date(MV_FinancialRatios$public_date, tryFormats = c("%Y-%m-%d")), "%Y-%m")

CARresults$f.began.month <- format(as.Date(CARresults$f.began, tryFormats = c("%Y-%m-%d")), "%Y-%m")

# Subtract one month and then extract the year and month
CARresults$f.began.prevmonth <- format(CARresults$f.began - months(1), "%Y-%m")

CARresults <- merge(CARresults, MV_FinancialRatios, by.x = c("ciq.gvkey", "f.began.prevmonth"), by.y = c("gvkey", "public_date_month"), all.x = TRUE)

# Replace observations which do not pass the diagnostic tests, adjust as needed per model
# CARresults <- CARresults[-68,]

cols_to_check <- c(
  "f.duration",
  "f.severity",
  "f.prev.customer",
  "f.prev.supplier",
  "cash_ratio",
  "roa",
  "debt_assets",
  "roe",
  "bm",
  "Beta"
)

CARresults <- CARresults[complete.cases(CARresults[cols_to_check]), ]

lm3 <-  lm(data = CARresults, 
           CAR.value ~ 
             f.duration 
           + f.severity # Cancel in parsimonious
           + f.prev.customer # Cancel in parsimonious
           + f.prev.supplier
           + cash_ratio
           + roa # Cancel in parsimonious
           + debt_assets
           + roe # Cancel in parsimonious
           + bm
           + Beta # Cancel in parsimonious
           )

vif(lm3)
plot(lm3, 4)

# --------------------> SWITCH: ADJUST NAME TO CHOSEN EVENT WINDOW, STORE VALUES, AND RERUN THE CODE IN THIS SECTION FOR OTHER WINDOWS #####
# For example, if you ran a (-12;12) event study, store lm3 in an object called "lm3.12" before repeating the process fo other windows
lm3.12 <- lm3
lm3.12_robust <- coeftest(lm3, vcov. = NeweyWest(lm3))

stargazer(lm3.3_robust, lm3.6_robust, lm3.9_robust, lm3.12_robust,
          type = "text", 
          report = "vc*t",
          column.labels = c("(-3,3)", "(-6,6)", "(-9,9)", "(-12,12)"),
          summary = TRUE
)

stargazer(lm3.3, lm3.6, lm3.9, lm3.12, 
          type = "text",
          report = "vc*t",
          column.labels = c("(-3,3)", "(-6,6)", "(-9,9)", "(-12,12)"),
          summary = TRUE
)

CARresults <- CARresults[, c(
  "CAR.value",
  "f.duration",
  "f.severity",
  "f.prev.customer",
  "f.prev.supplier",
  "cash_ratio",
  "roa",
  "debt_assets",
  "roe",
  "bm",
  "Beta"
)]

stargazer(CARresults,
          type = "latex",
          summary = T,
          summary.stat = c("N", "mean", "p25", "median", "p75", "sd")
)

##### Diagnostics #####

# Variance Inflation Factors
vif(lm3)

# Influential Outliers
plot(lm3, 4)

# Heteroscedasticity
bptest(lm3)

# Autocorrelation
dwtest(lm3)

# Normal distribution of residuals
lillieTest(residuals(lm3))

# Diagnostic Plots
plot(lm3, 1:6)



