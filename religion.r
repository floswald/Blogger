


require(RCurl)
require(reshape)        
require(ggplot2)
relurl  <- getURL("https://docs.google.com/spreadsheet/pub?key=0AnOrv_MIRexjdG9MeEdTc3liVVBnaHM0UVJwSmpXSWc&single=true&gid=2&output=csv")
rel  <- read.csv(textConnection(relurl))
names(rel) <- c("region","county","goodHealth","poorHealth","CIh","religion","NoReligion","CIr")
# check if good health + poor health = 100 %
sum(rel$goodHealth+rel$poorHealth==100)
nrow(rel)
mrel <- melt(rel[ ,c(1,2,4,5,6,7,8)],id=c(1,2,4))


mrel <- melt(rel[ ,c(1,2,4,6)],id=c(1,2,3))
cast(mrel,formula = region + poorHealth ~ variable, fun=mean,na.rm=T,margins="region")
