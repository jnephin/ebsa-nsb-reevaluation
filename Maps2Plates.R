#This code takes a folder of images and makes them into a plate of 4 with a,b,c,d annotated
#if there are fewer than 4 images, it will make the plate of 2 or 3
#Katie Gale, Nov 2017 updated 4 Jan 2018
# Edited by J Nephin 4 Jan 2018

#---------------------------------------------------------------------------------------#
# packages
library("magick")

#set wd
setwd('..')
setwd(file.path(getwd(),"Output/Maps"))


# create output directory
out <- "Plates"

# list input directories
dirs <- list.dirs(path="Plates/InputMaps", full.names = F)[-1]


#---------------------------------------------------------------------------------------#
# Plate images function
plate <- function(out, group, files, crop, textLoc){
  for (i in seq(1,length(files),4)){
    image1<-NULL
    image2<-NULL
    image3<-NULL
    image4<-NULL
    together<-NULL
    together2<-NULL
    together<-NULL
    image1<-image_read(files[i])
    image1<-image_crop(image1, crop )
    if(!is.na(files[i+1])){
      image2<-image_read(files[i+1])
      image2<-image_crop(image2, crop)
      image1<-image_annotate(image1, "A", size=70, location=textLoc)
      image2<-image_annotate(image2, "B", size=70, location=textLoc)
    } 
    if(!is.na(files[i+2])){
      image3<-image_read(files[i+2])
      image3<-image_crop(image3, crop)
      image3<-image_annotate(image3, "C", size=70, location=textLoc)
    } 
    if(!is.na(files[i+3])){
      image4<-image_read(files[i+3])
      image4<-image_crop(image4, crop)
      image4<-image_annotate(image4, "D", size=70, location=textLoc)
    }
    # append images 1 and 2
    together<-image_append(c(image1, image2))
    # append images 3 and 4 if present
    if(is.object(image3) &!is.object(image4)){
      together2<-image3
    } else if(is.object(image3)&is.object(image4)){
      together2<-image_append(c(image3, image4))
    } else {
      together2<-NA
    }
    # append all together
    if(is.object(together) & is.object(together2)){    
      together3<-image_append(c(together, together2),stack=T)
    } else {
      together3<-together
    }
    # write image
    image_info(image1)
    image_write(together3, paste0(out,"/",group,"_",i,".png"), format="png")
  }
}

#---------------------------------------------------------------------------------------#
# make plates by directory
for (d in dirs){
  
  #image processing parameters
  if(d =="Productivity") {
    crop<- c("1535x1515")
    textLoc<-c("+80+70")
  } else {
    crop<- c("1395x1515")
    textLoc<-c("+80+70")
  }

  # list files
  files <- list.files(path = file.path("Plates/InputMaps",d), 
                      pattern = "*.tif", full.names = T)
  
  # reorder
  if(d == "Fish") files <- c(files[1:10],files[16:17],files[11:15],files[18:20])
  if(d == "Productivity") files <- rev(files)
  if(d == "Inverts") files <- c(files[1],files[3:10],files[12],files[11],files[2])
  
  # plate images
  plate(out=out, group=d, files=files, crop=crop, textLoc=textLoc)
  
}



  

