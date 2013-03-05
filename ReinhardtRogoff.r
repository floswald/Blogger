

library(xlsx)
cdat <- read.xlsx(file="~/datasets/ReinhardRogoff/18_data.xls",sheetName="Aut.Ita.UK.US")
cdat <- cdat[,-c(5,6,7)]
library(reshape)
library(ggplot2)
m <- melt(cdat,c("Year","Country"))
p <- ggplot(m,aes(x=Year,y=value)) + geom_line(aes(color=variable),size=1) + facet_wrap(~Country,ncol=1) + scale_y_continuous(name="Public (Public and Private) debt to GDP (GNP) ratios") + theme(legend.position="top")
png(file="~/Dropbox/code-backup/Rstuff/RandomCodeSnippets/country.png",width=600,height=600)
p
dev.off()

p2 <- ggplot(subset(m,Countr %in% c("US","Italy","Spain")),aes(x=Year,y=value)) + geom_line(aes(color=variable),size=1) + facet_wrap(~Country,ncol=1) + scale_y_continuous(name="Public (Public and Private) debt to GDP (GNP) ratios") + theme(legend.position="top")
png(file="~/Dropbox/code-backup/Rstuff/RandomCodeSnippets/spain.png",width=600,height=600)
print(p)
dev.off()
