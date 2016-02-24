library(reshape2)
library(ggplot2)
library("RColorBrewer")

annual = read.csv("annual_analysis.csv")
extreme.to.large = annual[,3] - annual[,4]
large.to.all = annual[,2] - annual[,3]
annual = cbind(annual[,c(1,4)], extreme.to.large, large.to.all)
annual = melt(annual, id.vars = "Year")
colors = brewer.pal(name="Blues", n=nlevels(annual$variable))
names(colors) = rev(levels(annual$variable))
ggplot(annual,aes(x=Year,value,fill=variable)) + geom_bar(stat="identity") + 
  scale_fill_manual(values = colors, name="Magnitude", labels=c("M>6", "4<M<=6", "M<=4")) +  
  ylab("count") + ggtitle("Annual Floods") + theme_bw()


monthly = read.csv("monthly_analysis.csv")
extreme.to.large = monthly[,3] - monthly[,4]
large.to.all = monthly[,2] - monthly[,3]
monthly = cbind(monthly[,c(1,4)], extreme.to.large, large.to.all)
monthly = melt(monthly, id.vars = "Month")
colors = brewer.pal(name="Blues", n=nlevels(monthly$variable))
names(colors) = rev(levels(monthly$variable))
ggplot(monthly,aes(x=Month,value,fill=variable)) + geom_bar(stat="identity") + 
  scale_fill_manual(values = colors, name="Magnitude", labels=c("M>6", "4<M<=6", "M<=4")) +  
  ylab("count") + ggtitle("Monthly Floods") + scale_x_discrete(limits=1:12)
