##------------------------------------------------------------------------------------------------------------------
## Part 1
## Le but du script est de comparer la liste des fichiers laz préalbalement téléchargés 
## avec la liste des laz nouvellement téléchargés sur IGN
## On retire les duplicats pur ne pas télécharger une nouvelle fois les mêmes tuiles
##------------------------------------------------------------------------------------------------------------------

library(data.table)

# Get a list of all files in the folder
fpath <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/las"
flist<- list.files(path = fpath, full.names = TRUE)
write.csv(flist, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/flist_dwld.csv")

#List of las files already downloaded
flist<-fread("/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/flist_dwld.csv")

#Complete list of last files to download but with duplicates (las already downloaded)
dlist<-fread("/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/dlist_dwld.txt",header = F)

#Function to remove the text before /
remove_before_last_slash <- function(line) {
  if (is.character(line)) {
    parts <- unlist(strsplit(line, "/"))
    return(tail(parts, n = 1))
  } else {
    return(NA)  # Return NA for non-character elements
  }
}

# Apply the function to list
flist_name <- sapply(flist$V2, remove_before_last_slash)
dlist_name <- sapply(dlist$V1, remove_before_last_slash)

# Combine the two lists and remove duplicates
alist<-c(flist_name, dlist_name)
clist <- unique(alist)

#Put back the url
base_url <- "https://storage.sbg.cloud.ovh.net/v1/AUTH_63234f509d6048bca3c9fd7928720ca1/ppk-lidar/QK/"
final_list <- paste0(base_url, clist)

# Save the list
writeLines(final_list, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/complete_list.txt")

##------------------------------------------------------------------------------------------------------------------
## Part 2
## La conversion des laz en CHM_tiff peut être très longue et stopper brutalement (script lidar_extract). 
## Le but du script ci-dessous est de comparer la liste des laz nouvellement téléchargés sur IGN et la liste des tif 
## On retire les fichiers de la liste laz à transformer qui sont déjà convertis pour ne pas perdre de temps dans la conversion
##------------------------------------------------------------------------------------------------------------------

library(data.table)

# Get a list of all files in the folder tif_chm
fpath <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/tif_chm"
flist<- list.files(path = fpath, full.names = TRUE)
write.csv(flist, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/tiff.csv")

#List of tif files
flist<-fread("/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/tiff.csv")

#Complete list of last files withiut duplicates
dlist<-fread("/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/complete_list.txt",header = F)

#Function to remove the text before /
remove_before_last_slash <- function(line) {
  if (is.character(line)) {
    parts <- unlist(strsplit(line, "/"))
    return(tail(parts, n = 1))
  } else {
    return(NA)  # Return NA for non-character elements
  }
}

# Apply the function to list
flist_name <- sapply(flist$V2, remove_before_last_slash)
dlist_name <- sapply(dlist$V2, remove_before_last_slash)

# Combine the two lists and remove duplicates
flist_name<- gsub("_chm.tif", "__", flist_name)
dlist_name<- gsub(".laz", "__", dlist_name)

flist_name1<-as.data.frame(flist_name)
dlist_name1<-as.data.frame(dlist_name)
flist_name1$flist_name
dlist_name1$dlist_name
filtered_dlist <- dlist[!dlist_name1$dlist_name %in% flist_name1$flist_name, drop = FALSE]
print(filtered_dlist)

# Save the list
write.csv(filtered_dlist, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/complete_list2.txt")


# Move files
filtered_list_path <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/complete_list2.txt"
source_folder <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/las/"
destination_folder <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/dwld3/"
filtered_dlist <- fread(filtered_list_path, header = FALSE)

for (file_name in filtered_dlist$V1) {
  # Construct the full path for source and destination
  source_path <- file.path(source_folder, file_name)
  destination_path <- file.path(destination_folder, file_name)
  
  # Move the file
  if (file.exists(source_path)) {
    file.rename(source_path, destination_path)
  } else {
    warning(paste("File does not exist:", source_path))
  }
}
warnings()

## other useful bit of codes (if needed)
library(dplyr)
filtered_dlist <- sapply(filtered_dlist$V1, remove_before_last_slash)
names(dlist_name1)[names(dlist_name1) == "dlist_name"] <- "name"
names(flist_name1)[names(flist_name1) == "flist_name"] <- "name"

flist_name1$name
dlist_name1$name
identical(dlist_name1, flist_name1)

write.csv(flist_name1$name,"/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/flisteNAME.csv")
write.csv(dlist_name1$name,"/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/dlisteNAME.csv")

unique_names <- setdiff(dlist_name1$name, flist_name1$name)
dlist_filtered <- dlist_name1[dlist_name1$name %in% unique_names, ]
print(dlist_filtered)

