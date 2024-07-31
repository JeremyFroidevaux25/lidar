
##------------------------------------------------------------------------------------------------------------------
## Part 1
## extract mean, sd and cv of canopy height at sites monitored in the Mont-Blanc valley 
## (for both the grid points of the shrubland and the points of the vegetation survey)
##------------------------------------------------------------------------------------------------------------------

library(terra)
library(sf)

# Load the point shapefile (coordinates of the sites monitored in the Mont-Blanc)
veg<- st_read("/Users/jeremyfroidevaux/Documents/Herbiland/GIS/Landus_placette_veg/placetteveg_locL93.shp")
st_crs(veg)
veg<-st_transform(veg, crs = 2154) 
vegt <- vect(veg)
st_crs(vegt)

# Load the point shapefile (coordinates of the points in the shrubland)
land<- st_read("/Users/jeremyfroidevaux/Library/CloudStorage/GoogleDrive-jfroidevaux@creamontblanc.org/Drive partagés/prototool/herbiland/analyses/landus/data/grille_MB/grid_MB_lande.shp")
st_crs(land)
land<-st_transform(land, crs = 2154) 
landt <- vect(land)
st_crs(landt)

# Load and stack the CHM raster data
# There are two folders because the final tiff file is too large when attempting to merge all tiff files at once
path <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/tif_chm1" #tif
raster_files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
raster_list <- lapply(raster_files, rast)

chm1 <- do.call(merge, raster_list)
st_crs(chm1)
plot(chm1)
plot(vegt,add=T)
plot(landt,add=T)

path <- "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/tif_chm2" #tif
raster_files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
raster_list <- lapply(raster_files, rast)

chm2 <- do.call(merge, raster_list)
st_crs(chm2)
plot(chm2)
plot(vegt,add=T)
plot(landt,add=T)

# Calculate mean and sd values of canopy height in a 20 x 20 m grid
chm_recl1 <- chm1
chm_recl1[chm_recl1 < 0] <- 0
chm_recl1[chm_recl1 > 1.5] <- NA
plot(chm_recl1)
plot(vegt,add=T)

chm_recl2 <- chm2
chm_recl2[chm_recl2 < 0] <- 0
chm_recl2[chm_recl2 > 1.5] <- NA
plot(chm_recl2)
plot(vegt,add=T)

chm_mean1<- aggregate(chm_recl1, fact = c(20, 20), fun = function(x) mean(x, na.rm = TRUE))
chm_sd1<- aggregate(chm_recl1, fact = c(20, 20), fun = function(x) sd(x, na.rm = TRUE))
chm_mean2<- aggregate(chm_recl2, fact = c(20, 20), fun = function(x) mean(x, na.rm = TRUE))
chm_sd2<- aggregate(chm_recl2, fact = c(20, 20), fun = function(x) sd(x, na.rm = TRUE))

#Merge the two tiles
chm_mean <- merge(chm_mean1, chm_mean2)
chm_sd  <- merge(chm_sd1, chm_sd2)


# Extract values at point locations : veg shapefile
extracted_values_mean <- extract(chm_mean,vegt)*100
extracted_values_sd <- extract(chm_sd,vegt)*100

# Add the extracted values to the point shapefile as new columns : veg shapefile
veg$CHMmean <- extracted_values_mean[,2]
veg$CHMsd <-extracted_values_sd[,2]
veg$CHMcv <- extracted_values_sd[,2]/extracted_values_mean[,2]*100
veget<-as.data.frame(veg)
write.csv(veget,"/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/chm_20m.csv")

# Extract values at point locations : land shapefile
extracted_values_mean <- extract(chm_mean,landt)*100
extracted_values_sd <- extract(chm_sd,landt)*100

# Add the extracted values to the point shapefile as new columns : land shapefile
land$CHMmean <- extracted_values_mean[,2]
land$CHMsd <-extracted_values_sd[,2]
land$CHMcv <- extracted_values_sd[,2]/extracted_values_mean[,2]*100
landet<-as.data.frame(land)
write.csv(landet,"/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/chm_20m_naomit.csv")

#Check the number of NAs
nbna <- sum(is.na(landet$CHMmean))
propna <- mean(is.na(landet$CHMmean))*100

#Create and save a high-res CHM model (at 1m resolution)
chm_meanR <- merge(chm_recl1, chm_recl2)
plot(chm_meanR)
terra::writeRaster(chm_meanR, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/chm_1m.tif", overwrite=TRUE)

terra::writeRaster(chm_mean, "/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/chm_mean_20m_naomit.tif", overwrite=TRUE)
terra::writeRaster(chm_sd, filename="/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/chm_sd_20m_naomit.tif",  overwrite=TRUE)
res <- res(chm_mean)
res 

##------------------------------------------------------------------------------------------------------------------
## Part 2
## Comparison of vegetation height (mean and sd) between LiDAR and vegetation survey
##------------------------------------------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(gridExtra)

vegfield<-fread("/Users/jeremyfroidevaux/Library/CloudStorage/GoogleDrive-jfroidevaux@creamontblanc.org/Drive partagés/prototool/herbiland/analyses/landus/data/releve_vegetation/landus_20231109.csv")

vegchm20<-fread("/Users/jeremyfroidevaux/Documents/Herbiland/Lidar/chm_20m.csv")
names(vegchm20)[names(vegchm20) == "V2"] <- "plot_project"
veg20<-merge(vegfield,vegchm20,by="plot_project")

#Plot mean
rsquared <- function(model) {
  summary(model)$r.squared
}

p20m<-ggplot(veg20, aes(x = hcan, y = CHMmean)) +
  geom_smooth(method = "lm", se = T,colour="grey") +
  geom_point() + 
  theme_minimal() +  # Set minimal theme
  theme(
    plot.background = element_rect(fill = "white"),  # Set background color to white
    panel.background = element_rect(fill = "white"),  # Set panel background color to white
    axis.line = element_line(color = "black"),  # Set axis line color to black
    axis.text = element_text(color = "black"),  # Set axis text color to black
    axis.title = element_text(color = "black")  # Set axis title color to black
  ) +
  labs(x = "Canopy height Field (cm)", y = "Canopy height Lidar (cm)", title = "Mean canopy height (20m)")  +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, 
           label = paste("Slope:", round(coef(lm(CHMmean ~ hcan, data = veg20))[2], 3),
                         "R-squared:", round(rsquared(lm(CHMmean ~ hcan, data = veg20)), 3)))
p20m

#Plot sd
rsquared <- function(model) {
  summary(model)$r.squared
}

p20s<-ggplot(veg20, aes(x = heterog, y = CHMsd)) +
  geom_smooth(method = "lm", se = T,colour="grey") +
  geom_point() + 
  theme_minimal() +  # Set minimal theme
  theme(
    plot.background = element_rect(fill = "white"),  # Set background color to white
    panel.background = element_rect(fill = "white"),  # Set panel background color to white
    axis.line = element_line(color = "black"),  # Set axis line color to black
    axis.text = element_text(color = "black"),  # Set axis text color to black
    axis.title = element_text(color = "black")  # Set axis title color to black
  ) +
  labs(x = "SD Canopy height Field (cm)", y = "SD Canopy height Lidar (cm)", title = "SD canopy height (20m)")  +
  annotate("text", x = Inf, y = Inf, hjust = 1, vjust = 1, 
           label = paste("Slope:", round(coef(lm(CHMsd ~ heterog, data = veg20))[2], 3),
                         "R-squared:", round(rsquared(lm(CHMsd ~ heterog, data = veg20)), 3)))
p20s

#Merge plots
combined_plot <- grid.arrange(p20m,p20s,ncol = 2)
combined_plot

# Test with a linear regression
M1<-lm(log(veg20$hcan+1)~veg20$CHMmean)
summary(M1)
shapiro.test(residuals(M1))
library(DHARMa)
simulationOutput <- simulateResiduals(fittedModel =M1)
plot(simulationOutput)
hist(residuals(M1))
plotQQunif(simulationOutput)
