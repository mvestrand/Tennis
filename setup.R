######################################################## 
# Install any required packages and setup the dataset
#

# Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")


# Unpack the dataset
zipfile <- "365722_715072_bundle_archive.zip"
datapath <- "./data"
if (!dir.exists(datapath)) {
  unzip(zipfile, exdir=datapath)
}