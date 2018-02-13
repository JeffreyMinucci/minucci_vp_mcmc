#load output files
#CONTROL
load(paste(vpdir_out_control,"tdarray_control.RData", sep = ""))
dim(tdarray_control)

# #neonic
# load(paste(vpdir_out_neonic,"tdarray_neonic.RData", sep = ""))
# dim(tdarray_neonic)

#TIME
load(paste(vpdir_output,"timearray.RData", sep = ""))
nrows<- length(timearray)
#rownames(tdarray)
#days, outputs, simulations


#read input files
#CONTROL
indata_control <- read.csv(file = paste(vpdir_in_control, "inputdata_control.csv", sep = ""), header = TRUE)
#cut out column "X"
del_col <- which(colnames(indata_control)=="X")
inputdata_control<- indata_control[,-del_col]



#indata_neonic <- read.csv(file = paste(vpdir_in_neonic, "inputdata_neonic.csv", sep = ""), header = TRUE)
#inputdata_neonic<- indata_neonic%>%select_if(is.numeric)%>%select(-1)


#extract input vectors from dataframe
for(i in 1:length(inputdata_control)){assign(names(inputdata_control)[i], inputdata_control[[i]])}
for(i in 1:length(inputdata_neonic)){assign(names(inputdata_neonic)[i], inputdata_neonic[[i]])}

#convert dataframe to list
#linputdata <- as.list(inputdata)
#withdraw miteimmtype from list
#listinput<- as.list(linputdata[c(1:5,7:16)]) 



