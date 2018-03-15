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
full_set <- rbind(round(neonic[,-1],2),interpolated)
full_set <- full_set[order(full_set$day),]
rownames(full_set) <- c(1:nrow(full_set))
full_set$Date <- seq.Date(from=as.Date("04/29/2015",format="%m/%d/%Y"),to=as.Date("05/27/2015",format="%m/%d/%Y"),by=1)
pollen_contam <- full_set[,c(12,2:11)]


#fit the 'missing' data point for FSR
FSR_spline <- splinefun(neonic$day,neonic[,5],method="natural")
missing_FSR <- FSR_spline(3)
pollen_contam[4,4] <- round(missing_FSR,2)

#add 0's for unobserved days following exposure event
zero_dates <- seq.Date(from=as.Date("05/28/2015",format="%m/%d/%Y"), to = as.Date("09/01/2015",format="%m/%d/%Y"),by=1)
zero_data <- cbind(Date = zero_dates, as.data.frame(matrix(rep(0,length(zero_dates)*10),ncol=10)))
colnames(zero_data) <- colnames(pollen_contam)
pollen_contam <- rbind(pollen_contam,zero_data)

#convert ng/g to g/g
pollen_contam[,-1] <- pollen_contam[,-1]/10^9

#set to correct date format
pollen_contam$Date <- as.character(pollen_contam$Date,format="%m/%d/%Y")

#Convert pollen values to nectar using a ratio from the literature
nectar_ratio <- .67 #Blacquiere et al 2012 - conclusion
nectar_contam <- cbind(Date=pollen_contam[,1],pollen_contam[,-1]*nectar_ratio)



#### Write pollen and nectar profiles for each site
data_processed_dir <- paste(vpdir,"data/processed/neonic_profiles/",sep="")

for(j in 1:10){
  to_write <- cbind(pollen_contam[,c(1,j+1)],nectar_contam[,j+1])
  write.table(to_write,paste(data_processed_dir,"neonic_profile_",j,".csv",sep=""),row.names=F,col.names=F,sep=",",quote=F)
}
