# ------------------------------------------------------------------------------------------------ #
# Spatial Querying of GEDI Version 2 Data in R
# The following R code example demonstrates how current GEDI Finder users can migrate their GEDI 
# Version 1 spatial querying workflow from GEDI Finder to NASA's CMR in R. This tutorial will show 
# users how to use NASA's Common Metadata Repository (CMR) to perform spatial [bounding box] queries 
# for GEDI Version 2 (V2) L1B, L2A, and L2B data and how to reformat the CMR response into a list of 
# links that will allow the user to retrieve the intersecting GEDI V2 sub-orbit granules directly 
# from the LP DAAC Data Pool. The script also shows how to export the list of links to a text file.  
# ------------------------------------------------------------------------------------------------ #
# Author: Cole Krehbiel
# Last Updated: 05/10/2021
# ------------------------------------------------------------------------------------------------ #
# Check for required packages, install if not previously installed
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr")}

# Import Packages
library(httr)

# Define Function to Query CMR
gedi_finder <- function(product, bbox) {
  
  # Define the base CMR granule search url, including LPDAAC provider name and max page size (2000 is the max allowed)
  cmr <- "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id="
  
  # Set up dictionary where key is GEDI shortname + version and value is CMR Concept ID
  concept_ids <- list('GEDI01_B.002'='C1908344278-LPDAAC_ECS', 
                      'GEDI02_A.002'='C1908348134-LPDAAC_ECS', 
                      'GEDI02_B.002'='C1908350066-LPDAAC_ECS')
  
  # CMR uses pagination for queries with more features returned than the page size
  page <- 1
  bbox <- sub(' ', '', bbox)  # Remove any white spaces
  granules <- list()          # Set up a list to store and append granule links to
  
  # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number
  cmr_response <- GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page))
  
  # Verify the request submission was successful
  if (cmr_response$status_code==200){
    
    # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number, format return as a list
    cmr_response <- content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry
    
    # If 2000 features are returned, move to the next page and submit another request, and append to the response
    while(length(cmr_response) %% 2000 == 0){
      page <- page + 1
      cmr_response <- c(cmr_response, content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry)
    }
    
    # CMR returns more info than just the Data Pool links, below use for loop to go through each feature, grab DP link, and add to list
    for (i in 1:length(cmr_response)) {
      granules[[i]] <- cmr_response[[i]]$links[[1]]$href
    }
    
    # Return the list of links
    return(granules)
  } else {
    
    # If the request did not complete successfully, print out the response from CMR
    print(content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$errors)
  }
}

# User-provided inputs (UPDATE FOR YOUR DESIRED PRODUCT AND BOUNDING BOX REGION OF INTEREST)
product <- 'GEDI02_A.002'           # Options include 'GEDI01_B.002', 'GEDI02_A.002', 'GEDI02_B.002'
bbox <- '-75.65,-55.62,-66.95,-17.58'  # bounding box coordinates in LL Longitude, LL Latitude, UR Longitude, UR Latitude format


# Call the gedi_finder function using the user-provided inputs
granules <- gedi_finder(product, bbox)
print(sprintf("%s %s Version 2 granules found.", length(granules), product))

# Export Results
# Set up output textfile name using the current datetime
outName <- sprintf("%s_GranuleList_%s.txt", sub('.002', '_002', product), format(Sys.time(), "%Y%m%d%H%M%S"))

# Save to text file in current working directory
write.table(granules, outName, row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')
print(sprintf("File containing links to intersecting %s Version 2 data has been saved to: %s/%s", product, getwd(), outName))

# ------------------------------------------------------------------------------------------------ #
# How to Set Up Direct Access the LP DAAC Data Pool with R
# The following R code will configure a netrc profile that will allow users to download data from
# an Earthdata Login enabled server.
# ------------------------------------------------------------------------------------------------ #
# Author: Cole Krehbiel
# Last Updated: 11/20/2018
# ------------------------------------------------------------------------------------------------ #
if ("sys" %in% rownames(installed.packages()) == FALSE) {install.packages("sys")}
if ("getPass" %in% rownames(installed.packages()) == FALSE) { install.packages("getPass")}
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr")}

# Load necessary packages into R
library(sys)
library(getPass)
library(httr)

# -----------------------------------SET UP ENVIRONMENT------------------------------------------- #
usr <- file.path(Sys.getenv("USERPROFILE"))  # Retrieve user directory (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}    # If no user profile exists, use home directory
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep) # Path to netrc file

# ----------------------------------CREATE .NETRC FILE-------------------------------------------- #
# If you do not have a  .netrc file with your Earthdata Login credentials stored in your home dir,
# below you will be prompted for your NASA Earthdata Login Username and Password and a netrc file
# will be created to store your credentials (home dir). Create an account at: urs.earthdata.nasa.gov
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov):")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}


# ------------------------------------------------------------------------------------------------ #
# How to Access the LP DAAC Data Pool with R
# The following R code example demonstrates how to configure a connection to download data from an
# Earthdata Login enabled server, specifically the LP DAAC Data Pool.
# ------------------------------------------------------------------------------------------------ #
# Author: Cole Krehbiel
# Last Updated: 11/14/2019
# ------------------------------------------------------------------------------------------------ #
# Check for required packages, install if not previously installed
if ("sys" %in% rownames(installed.packages()) == FALSE) {install.packages("sys")}
if ("getPass" %in% rownames(installed.packages()) == FALSE) { install.packages("getPass")}
if ("httr" %in% rownames(installed.packages()) == FALSE) { install.packages("httr")}

# Load necessary packages into R
library(sys)
library(getPass)
library(httr)
# ---------------------------------SET UP ENVIRONMENT--------------------------------------------- #
# IMPORTANT: Update the line below if you want to download to a different directory (ex: "c:/data/")
dl_dir <- Sys.getenv(getwd())                                 # Set dir to download files to
setwd(dl_dir)                                                # Set the working dir to the dl_dir
usr <- file.path(Sys.getenv("USERPROFILE"))                  # Retrieve home dir (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}                    # If no user profile exists, use home
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep)  # Path to netrc file

# ------------------------------------CREATE .NETRC FILE------------------------------------------ #
# If you already have a .netrc file with your Earthdata Login credentials stored in your home
# directory, this portion will be skipped. Otherwise you will be prompted for your NASA Earthdata
# Login Username/Password and a netrc file will be created to store your credentials (in home dir)
if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}

# ---------------------------CONNECT TO DATA POOL AND DOWNLOAD FILES------------------------------ #
# Below, define either a single link to a file for download, a list of links, or a text file
# containing links to the desired files to download. For a text file, there should be 1 file link
# listed per line. Here we show examples of each of the three ways to download files.
# **IMPORTANT: be sure to update the links for the specific files you are interested in downloading.

# 1. Single file (this is just an example link, replace with your desired file to download):
files <- "https://e4ftl01.cr.usgs.gov//GEDI_L1_L2/GEDI/GEDI02_A.002/2019.04.18/GEDI02_A_2019108002012_O01959_04_T03909_02_003_01_V002.h5"

# 2. List of files (these are just example links, replace with your desired files to download:
#files <- c("https://e4ftl01.cr.usgs.gov/MOLA/MYD09GA.006/2002.07.06/MYD09GA.A2002187.h10v04.006.2015149103018.hdf",
#           "https://e4ftl01.cr.usgs.gov/MOLT/MOD11A1.006/2000.03.09/MOD11A1.A2000069.h00v08.006.2015057070313.hdf")

# 3. Textfile containing links (just an example, replace with your text file location):
#files <- readLines("C:/datapool_downloads/URL_file_list.txt", warn = FALSE)

# Loop through all files
for (i in 1:length(files)) {
  filename <-  tail(strsplit(files[i], '/')[[1]], n = 1) # Keep original filename
  
  # Write file to disk (authenticating with netrc) using the current directory/filename
  response <- GET(files[i], write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = netrc), set_cookies("LC" = "cookies"))
  
  # Check to see if file downloaded correctly
  if (response$status_code == 200) {
    print(sprintf("%s downloaded at %s", filename, dl_dir))
  } else {
    print(sprintf("%s not downloaded. Verify that your username and password are correct in %s", filename, netrc))
  }
}
