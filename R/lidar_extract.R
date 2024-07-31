
## Données
## 
## Il faut aller sur https://geoservices.ign.fr/lidarhd
## et zoomer sur la carte pour obtenir le(s) fichier(s) `laz` d'intérêt.
##

## Package
## https://r-lidar.github.io/lidRbook/
library(lidR)
######################
## DSM ###############
######################
## Lecture du nuage de points X,Y,Z (DSM)
library(lidR)
library(pryr)
library(terra)

path <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/dwld"
las_files <- list.files(path, pattern = "\\.laz$", full.names = TRUE)

output_dir <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/tif_chm" # Define the output directory

for (file in las_files) {
  gc() # Collect garbage to free up memory
  
  las <- readLAS(file)
  if (!is.null(las)) {
    dtmraster <- rasterize_terrain(las, res = 1, algorithm = tin())
    nlas <- las - dtmraster
    #las_low <- filter_poi(nlas, Z <= 1.5)
    
    chmraster <- rasterize_canopy(nlas, res = 1, algorithm = p2r())
    
    output_file <- file.path(output_dir, paste0(basename(tools::file_path_sans_ext(file)), "_chm.tif"))
    
    writeRaster(chmraster, output_file, filetype = "GTiff", overwrite = TRUE, NAflag = NA)
  }
  
  print(mem_used())
}

# Final garbage collection
gc()


#################
##End of code 
#################


#Code Vincent
las <- readLAS("/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/new/LHD_FXX_1006_6551_PTS_C_LAMB93_IGN69.copc.laz")
str(las)

## Affichage du nuage
## plot(las, size = 3, bg = "white") => très lourd, ne pas faire
lasraster <- pixel_metrics(las, mean(Z), res=1) # sous forme de raster
plot_dtm3d(lasraster, bg = "white")


######################
## DTM ###############
######################
## Calcul du X,Y,Z du sol (DTM)
## C'est un calcul, pas forcément juste
## si la canopée est trop dense (le sol est invisible)
dtmraster <- rasterize_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtmraster, bg = "white")


######################
## CHM ###############
######################
## Calcul du X,Y,Z de la canopée (CHM)
## comme la différence entre le DSM et le DTM
## Option1 : directement sur les points
## mais quelques valeurs négatives !!
nlas <- laslow - dtmraster
chmraster <- rasterize_canopy(nlas, res = 1, algorithm = p2r())
col <- height.colors(25)
plot(chmraster, col = col)

## Option2 : on le fait sur les rasters?
## mais ça ne marche pas, nombreuses valeurs négatives  !!
#chmraster = lasraster - dtmraster

## Export pour QGIS
terra::writeRaster(chmraster, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/peclerey05_chm.tif",
                   filetype = "GTiff", overwrite = TRUE, NAflag=NA)


######################
## CLASSIFICATION ####
######################
## Classification faite par IA:
## - Non classé. (1)
## - Sol (2)
## - Végétation basse (3) 0## -50cm
## - Végétation moyenne (4) 50 cm## -1,50m
## - Végétation haute (5) +1,50 m
## - Bâtiment (6)
## - Eau (9)
## - Pont (17)
## - Sursol pérenne, (64)
## - Artefacts (65)
## - Points virtuels (66)
table(las$Classification)
## Calcul d'un raster avec argmax(Classification) par point du raster
Argmax <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
classificationraster <- pixel_metrics(las, ~Argmax(Classification), res=5)
plot(classificationraster)

## Export pour QGIS
terra::writeRaster(classificationraster, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/blaitiere01_lidar_recl.tif",
                   filetype = "GTiff", overwrite = TRUE, datatype='INT1U', NAflag=NA)


## Est-ce que la CHM est cohérente avec la classification ?
## Pas complètement, on a beaucoup de points dans les moustaches des boxplot
boxplot(split(as.matrix(chmraster), as.matrix(classificationraster)), ylim=c(0,5),
        ylab="estimated canopy height (lidR package)", xlab="IGN classification")
