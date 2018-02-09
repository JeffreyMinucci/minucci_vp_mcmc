######
#
# Code to develop pollen and nectar contamination files from field data
# using spline interpolation for unobserved days
#
# by: Jeffrey Minucci
#
######
library(foreach)

if(Sys.info()[4]=="DZ2626UJMINUCCI"){
  vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
}

#read raw data - unit = pollen clothianidin equivalents (ng g-1)
neonic <- read.csv(paste(vpdir,"data/raw/field_neonic_exposure.csv",sep=""))

#fit natural spline smoothers for each site
to_fit <- seq(1,max(neonic$day),1)[!(seq(1,max(neonic$day),1) %in% neonic$day)]

interpolated <- cbind("day"=to_fit,
                      foreach(i=1:10, .combine=cbind) %do% {
                        smoother <- splinefun(neonic$day,neonic[,i+2],method="natural")
                        out <- smoother(to_fit)
                        out[out<0] <- 0
                        round(out,2)
                      })
colnames(interpolated) <- colnames(neonic)[-1]


#Build set of observed values and interpolated values for pollen
full_set <- rbind(round(neonic[,-1],2),interpolated)[order(full_set$day),]
rownames(full_set) <- c(1:nrow(full_set))
full_set$Date <- seq.Date(from=as.Date("04/29/2015",format="%m/%d/%Y"),to=as.Date("05/27/2015",format="%m/%d/%Y"),by=1)
pollen_contam <- full_set[,c(12,2:11)]


### NOTE - need to interpolate the missing NA value

#Convert pollen values to nectar using a ratio from the literature
nectar_ratio <- .5

nectar_contam <- cbind(pollen_contam[,1],pollen_contam[,-1]*nectar_ratio)
