# This code takes a folder of images and makes them into a plate of 2 or more images
# Edited by J Nephin 4 Jan 2018

#---------------------------------------------------------------------------------------#
# packages
library("magick")
require(magrittr)

#set wd
setwd('..')
setwd(file.path(getwd(),"Output/Figures/Sensitivity"))


# create output directory
out <- "Plates"

# list input directories
dirs <- list.dirs(path="Plates/InputFigures", full.names = F)[-1]


#---------------------------------------------------------------------------------------#
# Birds 
d = "Birds"
# list files
files <- list.files(path = file.path("Plates/InputFigures",d), 
                    pattern = "*.tif", full.names = T)
# append images
crop<- geometry_area(251, 185, 0, 20)
row1 <- image_read(files[1:3]) %>% image_crop(crop) %>% image_append()
row2 <- image_read(files[4:6]) %>% image_crop(crop) %>% image_append()
row3 <- image_read(files[7:8]) %>% image_crop(crop) %>% image_append()
birds <- image_append(c(row1,row2,row3),stack=T)
image_write(birds, paste0(out,"/Birds_SensitivtyPlots.png"), format="png")

#---------------------------------------------------------------------------------------#
# Fish 
d = "Fish"
# list files
files <- list.files(path = file.path("Plates/InputFigures",d), 
                    pattern = "*.tif", full.names = T)
# append images
crop<- geometry_area(251, 185, 0, 20)
row1 <- image_read(files[1:5]) %>% image_crop(crop) %>% image_append()
row2 <- image_read(files[6:10]) %>% image_crop(crop) %>% image_append()
row3 <- image_read(files[11:15]) %>% image_crop(crop) %>% image_append()
row4 <- image_read(files[16:20]) %>% image_crop(crop) %>% image_append()
fish <- image_append(c(row1,row2,row3,row4),stack=T)
image_write(fish, paste0(out,"/Fish_SensitivtyPlots.png"), format="png")

#---------------------------------------------------------------------------------------#
# Inverts 
d = "Inverts"
# list files
files <- list.files(path = file.path("Plates/InputFigures",d), 
                    pattern = "*.tif", full.names = T)
# append images
crop<- geometry_area(251, 185, 0, 20)
row1 <- image_read(files[1:3]) %>% image_crop(crop) %>% image_append()
row2 <- image_read(files[4:6]) %>% image_crop(crop) %>% image_append()
row3 <- image_read(files[7:9]) %>% image_crop(crop) %>% image_append()
row4 <- image_read(files[10:12]) %>% image_crop(crop) %>% image_append()
inverts <- image_append(c(row1,row2,row3,row4),stack=T)
image_write(inverts, paste0(out,"/Inverts_SensitivtyPlots.png"), format="png")

#---------------------------------------------------------------------------------------#
# MarineMammals 
d = "MarineMammals"
# list files
files <- list.files(path = file.path("Plates/InputFigures",d), 
                    pattern = "*.tif", full.names = T)
# append images
crop<- geometry_area(251, 185, 0, 20)
row1 <- image_read(files[1:3]) %>% image_crop(crop) %>% image_append()
row2 <- image_read(files[4:6]) %>% image_crop(crop) %>% image_append()
row3 <- image_read(files[7:8]) %>% image_crop(crop) %>% image_append()
mm <- image_append(c(row1,row2, row3),stack=T)
image_write(mm, paste0(out,"/MarineMammals_SensitivtyPlots.png"), format="png")

#---------------------------------------------------------------------------------------#
# Diversity 
d = "Diversity"
# list files
files <- list.files(path = file.path("Plates/InputFigures",d), 
                    pattern = "*.tif", full.names = T)
# append images
crop<- geometry_area(251, 185, 0, 20)
row1 <- image_read(files[1:2]) %>% image_crop(crop) %>% image_append()
row2 <- image_read(files[3:4]) %>% image_crop(crop) %>% image_append()
div <- image_append(c(row1,row2),stack=T)
image_write(div, paste0(out,"/Diversity_SensitivtyPlots.png"), format="png")


#---------------------------------------------------------------------------------------#
# Productivity 
d = "Productivity"
# list files
files <- list.files(path = file.path("Plates/InputFigures",d), 
                    pattern = "*.tif", full.names = T)
# append images
crop<- geometry_area(405, 235, 0, 20)
prod <- image_read(files[1:2]) %>% image_crop(crop) %>% image_append(stack=T)
image_write(prod, paste0(out,"/Productivity_SensitivtyPlots.png"), format="png")
