# load up varroapop data into a 3d array dataframe ######

df <- read.table(paste(vpdir_out_control,"results",i,".txt", sep=""), header= FALSE, sep= "", 
                skip = 6, stringsAsFactors = FALSE, row.names=NULL)
dim(df)
nrows <- dim(df)[[1]] #this is dependent on the duration of the simulation as set in the comparison.vrp file
ncols <- dim(df)[[2]]

timearray<- array(data=NA, c(nrows))
timearray[2:nrows]<- df[2:nrows,1]
timearray<- as.Date(timearray,"%m/%d/%Y")
timediff <- timearray[3]-timearray[2]
timearray[1] <- timearray[2]-timediff
length(timearray)
ncols
outvar<- c("Date","Colony Size","Adult Drones","Adult Workers", "Foragers", "Capped Drone Brood", "Capped Worker Brood",
           "Drone Larvae", "Worker Larvae", "Drone Eggs", "Worker Eggs", "Free Mites", "Drone Brood Mites",
           "Worker Brood Mites", "Mites/Drone Cell", "Mites/Worker Cell", "Mites Dying", "Proportion Mites Dying",
           "Colony Pollen (g)", "Pollen Pesticide Concentration", "Colony Nectar", "Nectar Pesticide Concentration",
           "Dead Drone Larvae", "Dead Worker Larvae", "Dead Drone Adults", "Dead Worker Adults", "Dead Foragers",
           "Queen Strength", "Average Temperature (celsius)", "Rain")
length(outvar)


# read output files
#CONTROL

tdarray_control <- array(data=NA, c(nrows,ncols-1,Nsims))
dim(tdarray_control)
for (j in 1:Nsims) {
  df <- read.table(paste(vpdir_out_control,"results",i,".txt", sep=""), header= FALSE, sep= "", 
                  skip = 6, stringsAsFactors = FALSE, row.names=NULL, col.names = outvar)
  newarray <- df[,2:ncols]
  tdarray_control[1:nrows,1:(ncols-1),j] <- abind(newarray[1:nrows,1:(ncols-1)], along=3)
}
#print(tdarray_control)

#save(tdarray_control, file = paste(vpdir_out_control,"tdarray_control.RData", se = ""))
#rm(tdarray_control)

# 
# #neonic
# tdarray_neonic <- array(data=NA, c(nrows,ncols-1,Nsims))
# dim(tdarray_neonic)
# for (i in 1:Nsims) {
#   df <- read.table(paste(vpdir_out_neonic,"results",i,".txt", sep=""), header= FALSE, sep= "", 
#                    skip = 6, stringsAsFactors = FALSE, row.names=NULL)
#   newarray <- df[,2:ncols]
#   tdarray_neonic[1:nrows,1:(ncols-1),i] <- abind(newarray[1:nrows,1:(ncols-1)], along=3)
# }
# save(tdarray_neonic, file = paste(vpdir_out_neonic,"tdarray_neonic.RData", sep = ""))
# rm(tdarray_neonic)


if(i==1) save(timearray,file = paste(vpdir_output,"timearray.RData", sep = ""))
