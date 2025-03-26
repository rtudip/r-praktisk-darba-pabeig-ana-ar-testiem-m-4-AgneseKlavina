library(ggplot2)
library(tidyverse)
library(dplyr)
library(magrittr)
library(svglite)

kordat <- read.table("variants15.txt", dec=",", strip.white=TRUE, stringsAsFactors=TRUE, row.names=1, header=TRUE, sep="\t")
kordat[, 9:ncol(kordat)] <-lapply(kordat[, 9:ncol(kordat)], as.factor)

sink("results.txt")

summary(kordat[, 9:ncol(kordat)])

sl.by.b <-split(kordat$Slope, kordat$b)
print(sl.by.b)

kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm=TRUE)


standartnovirze<- sapply(kordat[, 9:ncol(kordat)], function(x) tapply(as.numeric(x), kordat$f, sd, na.rm=TRUE))
print(standartnovirze)

prockordat <- kordat %>% filter(adj.r.squared>ifelse(all(kordat$adj.r.squared>0), 0.7, -0.3))

prockordat$Slope <- sapply(prockordat$Slope, function(y) 1-1/y)
print(prockordat)
sink()

svg("scatter.svg")
plot(kordat$MAD, kordat$Average, xlab="MAD", ylab="Average", main="Scatter plot")
dev.off()

svg("boxplot.svg")
boxplot(Intercept~f, data=kordat, col=rainbow(length(unique(kordat$f))), main="boxplot no f faktra")
dev.off()