kordat <- read.table("variants15.txt", header=TRUE, sep="\t", dec=",", strip.white=TRUE, row.names=1)

kordat[, 9:ncol(kordat)] <-lapply(kordat[, 9:ncol(kordat)], as.factor)

sink("results.txt")
kopsavilkums <- lapply(kordat[, 9:ncol(kordat)], table)
print(kopsavilkums)

sl.by.b <-split(kordat$Slope, kordat$b)
print(sl.by.b)


kordat$Slope <-as.numeric(as.character(kordat$Slope))
kordat$Intercept <-as.numeric(as.character(kordat$Intercept))
kordat$adj.r.squared <-as.numeric(as.character(kordat$adj.r.squared))
kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm=TRUE)

library(dplyr)
standartnovirze<-kordat %>%
    group_by(f) %>%
    summarise(standartnovirze=sd(Intercept, na.rm=TRUE))
print(standartnovirze)

prockordat <- kordat %>%
    filter(ifelse(adj.r.squared>0, adj.r.squared>0.7, adj.r.squared>-0.3))

prockordat$Slope <- 1 - (1 / prockordat$Slope)
print(prockordat)

library(ggplot2)
scatter <- ggplot(kordat, aes(x = MAD, y = Average)) +
  geom_point() +
  labs(title = "Scatter plot", x = "MAD", y = "Average")
ggsave("scatter.svg", plot=scatter, width=8, height=8)

kordat <- read.table("variants15.txt", header = TRUE, sep = "\t", dec = ",", strip.white = TRUE, row.names = 1)
kordat$f <- as.factor(kordat$f)
box <- ggplot(kordat, aes(x = f, y = Intercept, fill = f)) +
  geom_boxplot() +
  labs(title = "Boxplot pec f faktora")
ggsave("boxplot.svg", plot=box, width=8, height=8)

sink()
