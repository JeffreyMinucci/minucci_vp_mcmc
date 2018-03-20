### Code to launch a varroapop session run in the GUI for debugging
#
#   Jeff Minucci
###

if(Sys.info()[4]=="DZ2626UJMINUCCI"){
  vpdir<-path.expand("d:/Git_files/minucci_vp_mcmc/")
  vrp_filename <- "default_jeff.vrp"
}

SimEnd <- "8/25/2015"
koc <- 60
kow <- 2
nsims <- 1
step_length <- 1
start_point <- NULL
verbose <- FALSE
debug <- FALSE
logs <- TRUE

#static parameter list
static_names <- c("SimEnd","koc","kow")
static_values <- c(SimEnd,koc,kow)
static_vars<- list(names = static_names, values = static_values)

#parameters to optimize via MCMC
optimize_names <- c("ICQueenStrength","IPollenTrips","INectarTrips",
                    "ICForagerLifespan")
#   Notes: ICForagerLifespan appears to be converted to integer by removing decimal places in VP
bound_l <- c(1,4,4,4) #lower bondary of the domain for each parameter to be optimized
bound_u <- c(5,30,48,16) #upper bondary of the domain for each parameter to be optimized
#scales <- (bound_u-bound_l)/10 #for now using the range divided by 10
optimize_vars<- list(names = optimize_names, bound_l = bound_l, bound_u = bound_u)


vpdir_in <- paste(vpdir, "input/", sep = "")
vpdir_out <- paste(vpdir, "output/vp_output/", sep = "")
vpdir_log <- paste(vpdir, "log/", sep = "")
vpdir_exe <- paste(vpdir, "bin/", sep = "")
vp_binary <- "VarroaPop.exe"
vp_field_data <- paste(vpdir,"data/raw/field_bee_areas.csv",sep="")
vp_field_initials <- paste(vpdir,"data/raw/field_initial_conditions.csv",sep="")
vpdir_weather <- paste(vpdir, "data/external/weather/",sep="")
vpdir_neonic_prof <- "D:/Git_files/minucci_vp_mcmc/data/processed/neonic_profiles/"
dir_structure = list(input = vpdir_in, output = vpdir_out, log = vpdir_log, exe_folder = vpdir_exe,
                     exe_file = vp_binary, field_pops = vp_field_data, field_initials = vp_field_initials,
                     weather = vpdir_weather, neonic_profiles = vpdir_neonic_prof)

#if scales of parameters to optimize is not given, set at 1/10th of range
if(is.null(optimize_vars[["scales"]])) 
  optimize_vars[["scales"]] <- (optimize_vars[["bound_u"]]-optimize_vars[["bound_l"]])/10


#load initial conditions
initial_conditions <- read.csv(dir_structure[["field_initials"]],stringsAsFactors=FALSE)

#load bee populations to fit
field_data <- read.csv(dir_structure[["field_pops"]])
bees_per_cm2 <- 1.45  #convert area of bees to individuals  
bee_pops <- as.matrix(field_data[,c("bees_cm2_5","bees_cm2_6","bees_cm2_8")]) * bees_per_cm2 
bee_initial <- field_data[,c("bees_cm2_4")] * bees_per_cm2
#NOTE: need to consider hive that split

#dates to sample populations for each site
dates <- field_data[,c("date_5","date_6","date_8")]
days_sampled <- sapply(dates, function(x) as.Date(x,format="%m/%d/%Y") - as.Date(field_data[,"date_4"],format="%m/%d/%Y")) 


#load functions
source(paste(vpdir,"src/01parameterize_simulation.R",sep = "")) 
source(paste(vpdir,"src/02write_input.R",sep = "")) 
source(paste(vpdir,"src/03simulate_w_exe.R",sep = ""))
source(paste(vpdir,"src/04read_output.R",sep = ""))
source(paste(vpdir,"src/05likelihood.R",sep=""))
source(paste(vpdir,"src/06propose_mh_step.R",sep=""))


###   1) Randomly generate one set of parameters for the initial step OR use end of previous run (if given)
i <- 1 #counter for results and log files
if(is.null(start_point)){
  inputdata <- generate_vpstart(static_vars[["names"]], static_vars[["values"]], #generates 1 row dataframe with starting parameter values
                                optimize_vars[["names"]], optimize_vars[["bound_l"]],
                                optimize_vars[["bound_u"]], verbose) 
} else inputdata <- start_point
static_params <- as.data.frame(inputdata[,!(colnames(inputdata) %in%  optimize_vars[["names"]])],stringsAsFactors=F)
colnames(static_params) <- colnames(inputdata)[!(colnames(inputdata) %in%  optimize_vars[["names"]])]

###   2) Write VP inputs
write_vp_input_sites_c(params = inputdata[1,], in_path = dir_structure[["input"]],init_cond=initial_conditions,
                       neonic_path = dir_structure[["neonic_profiles"]])


###   3) Run VP simulation
system.time(run_vp_gui(i,dir_structure[["exe_folder"]],dir_structure[["exe_file"]],
                              vrp_filename, input_name = "input_mcmc_1.txt",in_path = dir_structure[["input"]],
                              out_path = dir_structure[["output"]],dir_structure[["log"]],logs=logs,debug=TRUE))
