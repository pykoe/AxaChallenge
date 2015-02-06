
# data come from the axa challenge

library(dplyr)

trainData <- read.csv("./data/challenge_17_data/challenge_axa_train.csv", sep=";")

# Correlations/covariances among numeric variables in 
# data frame train. Use listwise deletion of missing data.

data <- head (trainData, 1000)

#data <- trainData

# identify all numeric columns -> ture / false vector
nums <- sapply(data, is.numeric)

#pairs(data[nums][1:4])


#corTab <- cor(data[nums][1:10], use="complete.obs", method="kendall") 
corTab <- cor(data[nums]) 
corTab[lower.tri(corTab, diag = T)] <- NA
library(lattice)

#Build the plot
rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")

levelplot(corTab, main="stage 12-14 array correlation matrix", xlab="", ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))


          
which(corTab > 0.5 && row != col, arr.ind = T)          
          
# var_8          8  18
# var_10        10  19
# var_10        10  20
# var_19        19  20
# age_prospect   3  22

with( trainData, hist (var_19))

with( trainData, plot (var_8, var_18))

with( trainData, qplot (var_8, var_18, geom='jitter'))
with( trainData[trainData$target==0,], qplot (var_8, var_18, geom='jitter'))

# on se limite au premieres variable 
cdata <- data[, 1:11]

distance <- dist(cdata[,1:4])

