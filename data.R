library(raster)
library(rgdal)
library(readr)
library(tidyverse) 
########################################## Response variable y presence/absence ############################################
# trees data presence/absence 
trees = read_csv("data/forest_inventory_Madagascar.csv")
trees <- trees[-which(is.na(trees$sp)),]
species = unique(trees$taxize)
nplot = length(unique(trees$plot))
nspecies = length(species)

# abundance : number of species present on each plot
y=rep(0,nplot)
for (i in 1:nplot){
  y[i] = sum(trees$plot == i)
}

# presence/absence of each species on each plot
PA = matrix(0,nplot,nspecies)
rownames(PA) = sort(unique(trees$plot))
colnames(PA) = species
for (i in 1:nplot){    
  for (j in 1:nspecies){
    idx = which(trees$taxize == species[j])
    PA[paste(trees$plot[idx]),j] = 1 
  }
}

# Number of plot on which each species is present to detect rare species
npres = sort(apply(PA,2,sum))

# Keep the 50 species present on larger number of plots
PA_50 = as.data.frame(PA[,names(npres)[(nspecies-49):nspecies]])


################################ Climatic covariables ##########################################################################

s <- stack("data/current.tif")
names(s) <- c(paste("tmin",1:12,sep=""),paste("tmax",1:12,sep=""),
              paste("prec",1:12,sep=""),paste("bio",1:19,sep=""),
              paste("pet",1:12,sep=""),"pet","cwd","ndm")

# plot the number of dry months for Madagascar : 
par(mar=c(3,3,1,1))
plot(s$ndm,col=terrain.colors(255))

# get intersting covariables 
clim_var = dropLayer(s, c(1:36,38,39,41:47,49,50,52:67))
names(clim_var) <- c("temp","prec","sais_temp","sais_prec","pet","cwd","ndm")

# spatial points of each plot
longlat = SpatialPoints(unique(cbind(trees$long,trees$lat)))
proj4string(longlat) <- CRS("+proj=longlat +ellps=clrk66")

# latlong to UTM38S projection 
xy = spTransform(longlat, CRS("+init=epsg:32738"))

# extract climatic data on each plot
clim =  raster::extract(clim_var,xy)
pos = unique(trees[,c("long","lat","plot")])
#clim2 = clim^2
#colnames(clim2)<-paste(colnames(clim),rep("2",ncol(clim)),sep="")
#data_clim <- as_tibble(cbind(clim,clim2,pos))
data_clim <- as_tibble(cbind(clim,pos))
nparam = ncol(data_clim)-3

# order plot
ord_data_clim = data_clim[sort(data_clim$plot, index.return=TRUE)$ix,]

#  reduced centred data
scaled_data_clim = as_tibble(cbind(scale(ord_data_clim[1:nparam]),ord_data_clim[(nparam+1):ncol(ord_data_clim)]))

# representation of plots on Madagacscar map 
plot(clim_var$temp)
points(xy,pch=3,cex=1.6)

# data frame with explicative and response variable 

df = tibble(plot = rep(c(1:nplot),nspecies), species = rep(c(1:nspecies), each = nplot), 
            PA =  c(PA[,1:nspecies]), Intercept = rep(1,nplot*nspecies))
df$temp <- unlist(replicate(nspecies,scaled_data_clim[,"temp"],simplify=F), use.names =T, recursive =T)
df$prec <- unlist(replicate(nspecies,scaled_data_clim[,"prec"],simplify=F), use.names =T, recursive =T)
df$sais_temp <- unlist(replicate(nspecies,scaled_data_clim[,"sais_temp"],simplify=F), use.names =T, recursive =T)
df$sais_prec <- unlist(replicate(nspecies,scaled_data_clim[,"sais_prec"],simplify=F), use.names =T, recursive =T)
df$pet <- unlist(replicate(nspecies,scaled_data_clim[,"pet"],simplify=F), use.names =T, recursive =T)
df$cwd <- unlist(replicate(nspecies,scaled_data_clim[,"cwd"],simplify=F), use.names =T, recursive =T)
df$ndm <- unlist(replicate(nspecies,scaled_data_clim[,"ndm"],simplify=F), use.names =T, recursive =T)
