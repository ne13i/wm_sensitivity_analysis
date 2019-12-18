##### Sensitivity analysis Graphs with Slider #####
#### Nebi Yesilekin #####
rm(list=ls())
#rm(list=setdiff(ls(), c("Workdir", "Outdir"))) #remove everthing except workdir and outdir
readClipboard()
library(plotly)
library(gapminder)
library(stringr)
library(sf)
library(maps)
library(data.table)
library(rgdal)
library(ggplot2)
library(rnaturalearth)
library(gtools)


Workdir <-  "D:\\Work\\RawOutputs"
setwd(Workdir)
Outdir <- "D:\\Work\\Analysis"

####getting aggregated average for each sell
parent <- basename(Workdir)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
parentname <- firstup(unlist(strsplit(parent, "_", fixed = TRUE))[3]) ### 3 season name
parentfolder <- dir()
#####Choosing Parent Folders ####
#parentfolder[][c(1,2,3,4,5,6,7,8)]
numpar <- length(parentfolder)
resultssens <- c()
for (j in 1:numpar){
#Locations each cells
kc <- paste0(Workdir, "\\", parentfolder[j])
kc
list.dirs(kc)
main <- list.dirs(kc)[1]
split_path <- function(x) if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))
main <- split_path(main)[1]
main
# counting number of first folders
mainfolder<-dir(kc)
mainfolder
###Choosing mainfolder Management types only rainfed or only irrigated
#### 1: irrigated 2: rainfed 0N  3: rainfed highN  4: rainfed lowN c(1,2,3,4)
mainfolder <- mainfolder[][c(1,2,3,4)]
mainfoldername <- unlist(strsplit(mainfolder, "_", fixed = TRUE))[1] ###irrig name 
#mainfolder <- mainfolder [sapply(mainfolder, function(x) length(list.files(x))>0)]
#mainfolder
number3 <- length(mainfolder)


results3 <- c()
result<-c()
resultsw <- c()
for (k in 1:number3) {
  klm <- c()
  #matrix for reading csv colums
  cd <-paste0(kc, '\\', mainfolder[k])
  cd
  filename <- dir(cd)
  filename
  csvpath <- paste0(cd,'\\', filename)
  content <- read.csv(csvpath, header = T, sep = ',', row.names = NULL)
  colnames(content) <- c(colnames(content)[-1],NULL)
  ###excluding rows with 2018
  #rowsof2018 <- grep("2018", gsub(".{3}$", "", content[,17]))
  #content <- content[-rowsof2018,]
  #years <- content[2:35, 15]
  
  ### for meher season
 ## for ( c in 1:nrow(content)){
 ## if(gsub("^.{4}", "", content[c,22])<135){
 ## content[c,22] <- content[c,22]+365
 ## }
 ##   else{
 ##     next}
 ## }
   
  #years <- gsub(".{3}$", "", content[1:35, 17])  ##with 2018
  years <- gsub(".{3}$", "", content[1:34, 17]) ###without 2018
  #years <- content[2:35, 15]
  #years <- gsub(".{3}$", "", content[2:36, 17])
  n <- 34
  averagecell <- data.frame(content[seq(1,nrow(content),n),1],  ###Latitude
                            content[seq(1,nrow(content),n),2],  ### Longitude
                            content[seq(1,nrow(content),n),3],   ##Harvest Area each cell 
                            #aggregate(as.numeric(content[,3]),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),sum)[-1],  ##Harvested_area
                            content[seq(1,nrow(content),n), 17],  ###SDAT 
                            #round(aggregate(as.numeric(gsub("^.{4}", "", content[,18])),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),mean)[-1], digits = 0), #### PDAT 
                            as.integer(unlist(aggregate(as.numeric(gsub("^.{4}", "", content[,18])),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),mean)[-1])), #### PDAT 
                            as.integer(unlist(aggregate(as.numeric(gsub("^.{4}", "", content[,22])),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),mean)[-1])), #### HDAT 
                            aggregate(as.numeric(content[,25]),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),mean)[-1],   ##### Average_yield
                            aggregate(as.numeric(content[,81]),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),mean)[-1],    ### TMAXA 
                            aggregate(as.numeric(content[,82]),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),mean)[-1],    ###TMINA
                            aggregate(as.numeric(content[,86]),list(rep(1:(nrow(content)%/%n+1),each=n,len=nrow(content))),mean)[-1]      ####PRCP
  )
  
  
  
  
  names(averagecell) <- c("LATITUDE", "LONGITUDE", "HARVEST_AREA", "SDAT", "PDAT", "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP")
 ###for meher season 
 #for (p in 1:nrow(averagecell)){
 #  if(averagecell[p,6]>365){
 #    averagecell[p,6] <- averagecell[p,6] - 365
 #  }else{
 #    next
 #  }
 #}
  resultfiles <- paste0(Outdir, "\\", mainfolder[1], "all_.csv")
  #write.csv(averagecell,resultfiles)
  result <- rbind(result, averagecell)
  
  
}
# Create values for harvested area


deneme2 <- data.frame(result$LATITUDE, result$LONGITUDE, result$HARVEST_AREA, result$PDAT*result$HARVEST_AREA, result$HDAT*result$HARVEST_AREA, result$HWAM*result$HARVEST_AREA, result$TMAXA*result$HARVEST_AREA, result$TMINA*result$HARVEST_AREA, result$PRCP*result$HARVEST_AREA)
deneme4 <- data.frame(aggregate(list(deneme2$result.HARVEST_AREA, 
                                     deneme2$result.PDAT...result.HARVEST_AREA,
                                     deneme2$result.HDAT...result.HARVEST_AREA,
                                     deneme2$result.HWAM...result.HARVEST_AREA,
                                     deneme2$result.TMAXA...result.HARVEST_AREA,
                                     deneme2$result.TMINA...result.HARVEST_AREA,
                                     deneme2$result.PRCP...result.HARVEST_AREA), 
                                by=list(deneme2$result.LATITUDE, deneme2$result.LONGITUDE), 
                                FUN=sum))
colnames(deneme4) <- c("LATITUDE", "LONGITUDE", "HARVEST_AREA", "PDAT", "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP")

###Getting weighted average #####
deneme3 <- data.frame(deneme4$LATITUDE, deneme4$LONGITUDE, deneme4$HARVEST_AREA,
                      deneme4$PDAT/deneme4$HARVEST_AREA,
                      deneme4$HDAT/deneme4$HARVEST_AREA,
                      deneme4$HWAM/deneme4$HARVEST_AREA,
                      deneme4$TMAXA/deneme4$HARVEST_AREA,
                      deneme4$TMINA/deneme4$HARVEST_AREA,
                      deneme4$PRCP/deneme4$HARVEST_AREA)
colnames(deneme3) <- c("LATITUDE", "LONGITUDE", "HARVEST_AREA", "PDAT", "HDAT", "HWAM", "TMAXA", "TMINA", "PRCP")
### converting integer for decimal date average ###
deneme3$PDAT <- as.integer(deneme3$PDAT)
deneme3$HDAT <- as.integer(deneme3$HDAT)


resultssens <- rbind(resultssens, deneme3)

###Giving parent folder name to csv files #####
resultsfiles2 <- paste0(Outdir, "\\", main, "belg_cells.csv")
#write.csv(deneme3, resultsfiles2)


}




###appended 4 sensitivity runs fen_tot offset 0, 15, 30, 60
k <- resultssens
##conver all columns from factor to numeric
k[] <- lapply(k, function(x) as.numeric(as.character(x)))

###getting ETH border ### 
### creatinf geo sf
eth <- ne_states(country = "Ethiopia", returnclass = "sf")
class(eth)
eth_pp_sf <- st_as_sf(k, coords =c("LONGITUDE", "LATITUDE"), crs = 4326)

###creating offset number EXMPLE fen_tot 0 15 30 60 each repating number of totalnumber of deneme3
getoffset <- data.frame(parentfolder)
getoffset$parentfolder<- as.character(getoffset$parentfolder)

####Fertilizer applications ###
getoffset["fen_tot_offset"] <- gsub(paste0(parent,"_"), "", getoffset$parentfolder)
sub<- gsub("[^0-9]", "", getoffset$fen_tot_offset)
offset <- rep(sub, each=nrow(deneme3))

###creating offset number for planw 
getoffset["planw_offset"] <- gsub(paste0(parent,"_"), "", getoffset$parentfolder)
offset2 <- rep(getoffset$planw_offset, each=nrow(deneme3))
#getoffset$parentfolder <- str_sort(getoffset$parentfolder, decreasing = TRUE, numeric=TRUE)

#####Erain applications 
getoffset["erain_offset"] <- gsub(paste0(parent,"_"), "", getoffset$parentfolder)
getoffset$erain_offset <- sub("(.{1})(.*)", "\\1.\\2", getoffset$erain_offset)
offset3 <- rep(getoffset$erain_offset, each=nrow(deneme3))

k <- cbind(offset, k)
#k <- cbind(label_level, k)
#k$label_level <-NULL
#k$HWAM <- factor(k$HWAM, levels=1:length(label_level), labels  = as.character(label_level))

###parentfolder different cultivar """
#getoffset$parentfolder <- gsub("ET_MZ_eGHR_", "", getoffset$parentfolder)
#offset4 <- rep(getoffset$parentfolder, each=nrow(deneme3))



#eth_pp_sf$offset3 <- NULL
eth_pp_sf <- cbind(offset, eth_pp_sf)
eth_pp_sf$offset <- as.numeric(as.character(eth_pp_sf$offset))
### sort by slider frame (different sensitivity applications) ###
eth_pp_sf <- eth_pp_sf[order(eth_pp_sf$offset),]
#eth_pp_sf <- eth_pp_sf[with(eth_pp_sf, order(eth_pp_sf$offset)),]
eth_pp_sf$offset <- as.character(eth_pp_sf$offset)
row.names(eth_pp_sf) <- NULL

### giving axis name and adjusting grid size 

f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f")

x <- list(title="Longitude", titlefont= f)
y <- list(title="Latitude", titlefon =f)
m <- list(
  l = 50,
  r = 50,
  b = 100,
  t = 100,
  pad = 4
)
########################
##### ggplot and geom_sf 
frame1 <- eth_pp_sf$offset
colorhwam <- eth_pp_sf$HWAM
colorpdat <- eth_pp_sf$PDAT
colorhdat <- eth_pp_sf$HDAT
colorprcp <- eth_pp_sf$PRCP
resultvariables <- data.frame(colorhwam, colorpdat, colorhdat, colorprcp)
titlevariables <- c("Yield(kg/ha)", "Planting Day(DOY)", "Harvesting Day(DOY)", "Total Precipitation(mm)")
sensvariabletitle <- c("Fertilizer Offset Applications", "Rain Multiplier Applications", "Planting Window Applications")
prefix <- unlist(strsplit(parent, "_", fixed = TRUE))[4:5]
prefix <- paste0(prefix[1], "_", prefix[2],": ")
p <- ggplot()+
  geom_sf(data = eth, size=0.25, alpha=0.5, fill=NA) +
  geom_sf(data = eth_pp_sf, aes(frame = frame1, color = resultvariables[[1]]), shape=15, size=0.8333333) +
  #scale_color_gradientn(colours = hex) 
  scale_color_gradientn(colours = rainbow(5) 
                        #breaks =c(45,50, 55, 60, 65, 70,75,
                         #         80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160),
                        #expand_scale(mult = c(3,3)))
  )
p  



d <- ggplotly(p) %>%
  animation_opts(frame=4000, transition = 500)%>%
  layout(title = paste0("Ethiopia", " ",mainfoldername, " ", titlevariables[[1]],
                        " in ", parentname, " Season with ", sensvariabletitle[[1]]),
         xaxis = list(title = 'LONGITUDE',
                      zeroline = TRUE,
                      range = c(30, 52)),
         yaxis = list(title = 'LATITUDE',
                      range = c(2,16))
         )%>%
  animation_slider(active=0,
    currentvalue = list(prefix=prefix, font = list(color="red"))
    )
d



###different color options
### marker size 0.83 or 0.5 
###color option 2 
#hex <- c("#FF0000", "#FFA500", "#FFFF00", "#008000", "#9999ff", "#000066")

###exporting as png
###exporting animation as html
#export(d, file='fen_tot.png')

### give custom name to 
#htmlwidgets::saveWidget(d, paste0(Outdir, "\\", parent, ".html"))

### if it's all namagement remove mainfoldername ###
htmlwidgets::saveWidget(d, paste0(Outdir,"\\", parent,"_" ,colnames(eth_pp_sf)[2], ".html")) ###for PDAT









