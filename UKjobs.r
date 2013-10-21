

# UK jobs over time

library(ggplot2)
library(data.table)
library(xlsx)
library(reshape2)
library(zoo)

# get employee count data from ONS
# the website is at
# http://www.ons.gov.uk/ons/rel/lms/labour-market-statistics/june-2013/index-of-data-tables.html#tab-Jobs-tables
# I am using table JOBS03, register 1, "UK totals"
if (!file.exists("~/Downloads/ONS.xls")) download.file(url="http://www.ons.gov.uk/ons/rel/lms/labour-market-statistics/june-2013/table-jobs03.xls",destfile="~/Downloads/ONS.xls")
d <- data.table(read.xlsx(file="~/Downloads/ONS.xls",sheetIndex=1,rowIndex=c(4,7:146),colIndex=which(letters=="c"):(3*which(letters=="z")+which(letters=="h")),colClasses="numeric"))
d[,date := seq(from=as.Date("1978-06-01"),to=as.Date("2013-03-01"),by="3 months")]

# here's JOBS02 http://www.ons.gov.uk/ons/rel/lms/labour-market-statistics/june-2013/table-jobs02.xls
if (!file.exists("~/Downloads/ONS2.xls")) download.file(url="http://www.ons.gov.uk/ons/rel/lms/labour-market-statistics/june-2013/table-jobs02.xls",destfile="~/Downloads/ONS2.xls")
d2 <- data.table(read.xlsx(file="~/Downloads/ONS2.xls",sheetIndex=1,rowIndex=c(4,6:145),colIndex=which(letters=="c"):which(letters=="x"),colClasses="numeric"))
d2[,date := seq(from=as.Date("1978-06-01"),to=as.Date("2013-03-01"),by="3 months")]

# get completed housing units data from gov.uk
if (!file.exists("~/Downloads/ONS-houses.xls")) download.file(url="http://www.gov.uk/government/uploads/system/uploads/attachment_data/file/229690/LiveTable212.xls",destfile="~/Downloads/ONS-houses.xls",method="wget")

# index rows of housing spreadsheet
rows <- c(4,6:9)
r <- tail(rows,1) + 2	# start index of second batch
while (r < 180){
	rows <- c(rows,r:(r+3))
	r <- tail(rows,1) + 2
}

h <- data.table(read.xlsx(file="~/Downloads/ONS-houses.xls",sheetIndex=1,colIndex=which(letters=="j"):which(letters=="m"),rowIndex=rows,colClasses="numeric"))
h[,date := seq(from=as.Date("1978-01-01"),to=as.Date("2012-12-01"),by="3 months")]

# make a zoo time series object for a quick plot
z <- zoo(h[,1:4,with=FALSE],h[,date])
names(z) <- c("Private","H.Association","Public","All")
plot(z,main=sprintf("Houses Completed by Constructing Sector\nONS data: http://bit.ly/19mCrpx"))

# change names 
setnames(d,c("Construction.of.buildings","Real.estate.activities","Financial.service.activities.except.insurance.and.pension.funding"),c("Construction","Realestate","Financial"))
setnames(d2,c("Real.estate.activities"),c("Realestate"))
setnames(h,c("Private.Enterprise","Housing.Associations","Local.Authorities","All.Dwellings"),c("Private","HA","LA","All"))

# combine both datasets
# ASSUMPTION: 1978-06-01 = 1978-07-01
# i.e. I shift the time index in the jobs data back one month
h[,idate := 1:140]
d[,idate := 3:142]
d2[,idate := 3:142]
setkey(d,idate)
setkey(d2,idate)
setkey(h,idate)
dh <- d[h]
d2h <- d2[h]
dh[,date := NULL]
setnames(dh,"date.1","date")
setnames(d2h,"date.1","date")

# make some lagged variables

#d[,dateid := 1:nrow(d)]
#setkey(d,dateid)
d[,c("L.construct","L.realest") := d[.(idate-1)][,list(Construction,Realestate)]]
d[,c("d.construct","d.realest") := list(c(0,diff(Construction)),c(0,diff(Realestate)))]
d2[,c("L.construct","L.realest") := d2[.(idate-1)][,list(Construction,Realestate)]]
d2[,c("d.construct","d.realest") := list(c(0,diff(Construction)),c(0,diff(Realestate)))]

# change in employment over last year
d[date>"2011-12-01",list(realest=sum(d.realest),constr=sum(d.construct))]
d2[date>"2011-12-01",list(realest=sum(d.realest),constr=sum(d.construct))]

# jobs relative to summer 2008
rel2008 <- d[date>"2007-01-01",list(date=date,pctRealestate=Realestate/.SD[date=="2008-06-01",Realestate],pctConstruction=Construction/.SD[date=="2008-06-01",Construction],dRealest2008=Realestate-.SD[date=="2008-06-01",Realestate],dConstr2008=Construction-.SD[date=="2008-06-01",Construction]),.SDcols=c("date","Realestate","Construction")]
rel2008_2 <- d2[date>"2007-01-01",list(date=date,pctRealestate=Realestate/.SD[date=="2008-06-01",Realestate],pctConstruction=Construction/.SD[date=="2008-06-01",Construction],dRealest2008=Realestate-.SD[date=="2008-06-01",Realestate],dConstr2008=Construction-.SD[date=="2008-06-01",Construction]),.SDcols=c("date","Realestate","Construction")]

mrel <- melt(rel2008[,list(date,dRealest2008,dConstr2008)],id.vars="date")
prel <- ggplot(mrel,aes(x=date,y=value,color=variable)) + geom_line(size=1) + scale_y_continuous(name="thousands of employees relative to 2008-06-01")

mrel2 <- melt(rel2008_2[,list(date,dRealest2008,dConstr2008)],id.vars="date")
prel2 <- ggplot(mrel2,aes(x=date,y=value,color=variable)) + geom_line(size=1) + scale_y_continuous(name="thousands of employees relative to 2008-06-01",breaks=c(-300,-200,-100,0,25,50)) + ggtitle('JOBS02')


# melts

# both datasets together. color scale is not good to read, but facet lines up date variable nicely, so go with that.
mdh <- melt(dh[,list(date,Private,HA,LA,All,Construction,Realestate,Financial)],id.vars="date")
md2h <- melt(d2h[,list(date,Private,HA,LA,All,Construction,Realestate)],id.vars="date")

mdh$panel <- "houses built"
mdh[561:nrow(mdh),]$panel <- "jobs"
names(mdh)[2] <- "Sector"
mdh[mdh$panel=="houses built",]$value <- mdh[mdh$panel=="houses built",]$value / 1000

md2h$panel <- "houses built"
md2h[561:nrow(md2h),]$panel <- "jobs"
names(md2h)[2] <- "Sector"
md2h[md2h$panel=="houses built",]$value <- md2h[md2h$panel=="houses built",]$value / 1000

cbbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#000000", "#0072B2", "#D55E00")

p1 <- ggplot(mdh,aes(x=date,y=value,color=Sector,linetype=Sector)) + geom_line(size=1.2) + scale_y_continuous(name="thousands") + facet_wrap(~panel,nrow=2,scales="free_y") + scale_linetype_manual(values=c(rep("solid",4),rep("dotdash",3))) + scale_color_manual(values = cbbPalette)

# without Financial
p2 <- ggplot(subset(mdh,Sector != "Financial"),aes(x=date,y=value,color=Sector,linetype=Sector)) + geom_line(size=1.2) + scale_y_continuous(name="thousands") + facet_wrap(~panel,nrow=2,scales="free_y") + scale_linetype_manual(values=c(rep("solid",4),rep("dotdash",3))) + scale_color_manual(values = cbbPalette) + theme_bw()

p4 <- ggplot(md2h,aes(x=date,y=value,color=Sector,linetype=Sector)) + geom_line(size=1.2) + scale_y_continuous(name="thousands") + facet_wrap(~panel,nrow=2,scales="free_y") + scale_linetype_manual(values=c(rep("solid",4),rep("dotdash",3))) + scale_color_manual(values = cbbPalette) + theme_bw() + ggtitle('JOBS02')

# zoom into recent past
p3 <- ggplot(subset(mdh,date > "2000-01-01" & Sector != "Financial"),aes(x=date,y=value,color=Sector,linetype=Sector)) + geom_line(size=1.2) + scale_y_continuous(name="thousands") + facet_wrap(~panel,nrow=2,scales="free_y") + scale_linetype_manual(values=c(rep("solid",4),rep("dotdash",3))) + scale_color_manual(values = cbbPalette) 


# print plots
png(file="~/git/Blogger/UKjobs.png",height=500,width=600,pointsize=12)
print(p2)
dev.off()

png(file="~/git/Blogger/UKjobschange.png",height=400,width=600,pointsize=12)
print(prel)
dev.off()

png(file="~/git/Blogger/UKjobs2.png",height=500,width=600,pointsize=12)
print(p4)
dev.off()

png(file="~/git/Blogger/UKjobschange2.png",height=400,width=600,pointsize=12)
print(prel2)
dev.off()

