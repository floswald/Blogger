
rm(list=ls(all=T))
# using data on disk
# library(xlsx)
# cdat <- read.xlsx(file="~/datasets/ReinhardRogoff/18_data.xls",sheetName="Aut.Ita.UK.US")
# cdat <- cdat[,-c(5,6,7)]

# using data on google spreadsheets
require(RCurl)
relurl  <- getURL("https://docs.google.com/spreadsheet/pub?key=0AnOrv_MIRexjdFZlWnQxSmJNSDlJTllfU1BubFFTdnc&single=true&gid=0&output=csv")
cdat  <- read.csv(textConnection(relurl))
library(reshape)
library(ggplot2)
m <- melt(cdat,c("Year","Country"))
countries <- c("Austria","Greece","Italy","UK","US")
p <- ggplot(subset(m,Country %in% countries),aes(x=Year,y=value)) + geom_line(aes(color=variable),size=1) + facet_wrap(~Country,ncol=1) + scale_y_continuous(name="Public (Public and Private) debt to GDP (GNP) ratios") + theme(legend.position="top")
png(file="~/git/Blogger/country.png",width=600,height=600)
p
dev.off()

p2 <- ggplot(subset(m,Country %in% c("US","Italy","Spain")),aes(x=Year,y=value)) + geom_line(aes(color=variable),size=1) + facet_wrap(~Country,ncol=1) + scale_y_continuous(name="Public (Public and Private) debt to GDP (GNP) ratios") + theme(legend.position="top")
png(file="~/git/Blogger/spain.png",width=600,height=600)
print(p)
dev.off()
