
##------------------------------------------------------------------------------------------------------------------
## Download all the laz files at once from the list that we got from IGN
## https://geoservices.ign.fr/lidarhd
##------------------------------------------------------------------------------------------------------------------

library(httr)
library(readr)

#Define the file containing the list of URLs and the directory to save the downloaded files
links_file <- '/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/liste_dalle.txt'
download_directory <- '/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/dwld'

#Create the download directory if it doesn't exist
if (!dir.exists(download_directory)) {
  dir.create(download_directory)
}

#Function to download a file from a given URL
download_file <- function(url, directory) {
  file_name <- basename(url)
  destination <- file.path(directory, file_name)
  GET(url, write_disk(destination, overwrite = TRUE))
}

#Read the list of URLs from the text file
urls <- read_lines(links_file)

#Download each file from the list
for (url in urls) {
  download_file(url, download_directory)
}

