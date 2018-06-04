###################################################
### Isolated PV Model                      ########
### Designed for 1 year of hourly Data     ########
###################################################

library(ggplot2)

###Read Irrandaince and Load Files and combine
###Set path to directory containing input files
path = '../Data/GeneratedData/'
solar = read.csv(paste(path, "Hourly_Irradiance_Bakokwe.csv", sep=''), stringsAsFactors=FALSE)
load = read.csv(paste(path, "YearlyLoadHourly.csv", sep=""), stringsAsFactors=FALSE)
d = cbind(solar, load)
names(d) = c('hour', 'sun', 'load')

###Some example graphics generated with the ggplot library
#The whole time period: Irradiance
ggplot(d, aes(x = hour, y = sun)) + geom_line()
#The whole time period with labels: Irradiance
ggplot(d, aes(x=hour)) + geom_line(aes(y=sun), color='green4') + 
  ylab("Watts") + xlab("hour") + ggtitle("Bakokwe Irradiance Data") + 
  theme(plot.title = element_text(hjust = 0.5))
#The 1st 10 days: Irradiance
ggplot(d[1:240,], aes(x=hour[1:240])) + geom_line(aes(y=sun[1:240]), color='green4') + 
  ylab("Watts") + xlab("hour") + ggtitle("Bakokwe Irradiance Data") + 
  theme(plot.title = element_text(hjust = 0.5))
#The 1st 10 days: Load
ggplot(d[1:240,], aes(x=hour[1:240])) + geom_line(aes(y=load[1:240]), color='blue') + 
  ylab("Watts") + xlab("hour") + ggtitle("Rural Rwandan Load Data") + 
  theme(plot.title = element_text(hjust = 0.5))
#The 1st 5 days: Irradiance and Load
ggplot(d[1:120,], aes(x=d$hour[1:120])) + geom_line(aes(y=d$sun[1:120]*0.1), color='green4') + 
  geom_line(aes(y=d$load[1:120]), color='blue') + ylab("Watts") + xlab("Hour") +
  ggtitle("Bakokwe System Data") + theme(plot.title = element_text(hjust = 0.5))

#######################################################################
##############   The Optimization Program         #####################
#######################################################################
#The program implements an exhaustive grid search for the minimum
#cost combination of pv modules and batteries to serve a lighting 
#and minor auxillary load system. 
#######################################################################
#################    Model Parameters     #############################
#unit costs in $ per kW and $ per kWh for pv and battery respecitively
#battery depth is used as a constraint in the model
#pv.unit.watts is the discrete/incremental pv module size
#bat.inc is the discrete/incremental battery size

pvUnitCost=0.4
pvEfficiency = 0.1
pvUnitWatts = 200

batUnitCost=0.2
batInc = 600
batEfficiency = 0.8
batDepth = 0.55

############   OUtput Matrices   ##################
#Output matrices illustrating the model and used to 
#find optimal combination

costGrid = matrix(rep(0,100), nrow=10)
violGrid = matrix(rep(0,100), nrow=10)

#####################################################
##### Algorithm to fill the matrices ################

for (i in 1:10){
  pvSize = i
  pvCost = pvSize*pvUnitWatts*pvUnitCost
  for (j in 1:10){
    batSize = j
    batCapacity = batInc*batSize
    batCost = batUnitCost*batCapacity
    costGrid[[i,j]] = pvCost + batCost
    battery = list()
    battery[[1]]= batCapacity  #initally the battery is full
    for (k in 1:length(d$sun)){
      temp = battery[[k]] + pvSize*d$sun[[k]]*pvEfficiency - d$load[[k]]/batEfficiency
      if (temp>batCapacity) {
      battery[[k+1]] = batCapacity  
      } else {
      battery[[k+1]] = temp
      }
    }
    violGrid[[i,j]] = length(battery[battery<batDepth*batCapacity])
  }
}

########################################################
########## The Output and Analysis #####################
##Set the reliablity tolerance, the number of hours the battery is allowed to go below
##its recommended/planned depth of discharge, set above as batepth
constSet = costGrid
reliabilityTolerence = 0
constSet[violGrid>reliabilityTolerence]=NA
##Find the min cost 
m = min(constSet, na.rm=TRUE)
which(constSet == m, arr.ind=TRUE)

##########Visualizations of the Output  ###########################
heatmap(violGrid, Rowv=NA, Colv=NA, symm=TRUE)
heatmap(costGrid, Rowv=NA, Colv=NA, symm=TRUE)
heatmap(constSet, Rowv=NA, Colv=NA, symm=TRUE)
