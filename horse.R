#load required libraries
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(tidyverse)

#load the current files
RaceData <- fread('/Data/RaceData.csv')
HorseData <- fread('/Data/HorseData.csv')
PastPerf <- fread('/Data/PastPerf.csv')
JockeySR <- fread('/Data/Jockey_Stats.csv')
TrainerSR <- fread('/Data/Trainer_Stats.csv', header = T)

# Combine files
h = inner_join(PastPerf, HorseData, by = c("HORSEID"))
colnames(h)[colnames(h) == 'RACEID.x'] <- 'RACEID'
h = inner_join(h, RaceData, by = c("RACEID"))

# function to get class ratings
f1<- function(x){
  str_match(x, "(\\*)(.*?)(\\*)" )[3]
}

# extract class ratings
a <- dim(h)[1]
values1 <- lapply(h$RACENAME, f1  )
ClassRating1 <- data.frame(matrix(unlist(values1), nrow=a, byrow=T))
names(ClassRating1) <- "ClassRating"
h <- cbind.data.frame(h, ClassRating1)
h$ClassRating <- as.numeric(as.character(h$ClassRating))

#add Jockey SR
h$JockeyWinP <- JockeySR2023$WinP[match(h$JOCKEY, JockeySR2023$JOCKEY)]
#add Jockey Rating
h$JockeyRATE <- JockeySR2023$SRATE[match(h$JOCKEY, JockeySR2023$JOCKEY)]

#add Trainer SR
h$TrainerWinP <- TrainerSR2023$WinP[match(h$TRAINER, TrainerSR2023$TRAINER)]
#add Trainer Rating
h$TrainerRATE <- TrainerSR2023$SRATE[match(h$TRAINER, TrainerSR2023$TRAINER)]

h$JockeyWinP[is.na(h$JockeyWinP)] <- median(h$JockeyWinP, na.rm=TRUE)
h$JockeyRATE[is.na(h$JockeyRATE)] <- median(h$JockeyRATE, na.rm=TRUE)

h$TrainerWinP[is.na(h$TrainerWinP)] <- median(h$TrainerWinP, na.rm=TRUE)
h$TrainerRATE[is.na(h$TrainerRATE)] <- median(h$TrainerRATE, na.rm=TRUE)


# remove maidens
ClassType <- c("Maiden","Maiden Hurdle","Maiden & Jumpers","Maiden Steeple")
h <- subset(h, !(CLASS %in% ClassType))

####################################################################################################

## Explore and Visualize the data

library(ggplot2)

# sprint distances
ggplot(data = h, aes(x = bp, y = win_rate, fill = win_rate)) +
  geom_bar(width = 0.8, stat = "identity") +
  ggtitle('Win Rate by BP - Sprint Distances (up to 1200m)') +
  scale_x_continuous(name = 'BP', breaks = seq(0, 10, 1), limits=c(0, 11), expand=c(0, 0))  +
  scale_y_continuous(name = 'Win Rate', breaks = seq(0, 13, 2), limits = c(0,13), expand = c(0,0)) +
  scale_fill_gradient('Win Rate', low = "lightblue", high = "darkblue") +
  theme(legend.position = 'right',
        panel.background = element_rect(fill = 'lightgray'),
        plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=8),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8))

# staying distances
ggplot(data = h1, aes(x = bp, y = win_rate, fill = win_rate)) +
  geom_bar(width = 0.8, stat = "identity") +
  ggtitle('Win Rate by BP - Staying Distances (2400m+)') +
  scale_x_continuous(name = 'BP', breaks = seq(0, 10, 1), limits=c(0, 11), expand=c(0, 0))  +
  scale_y_continuous(name = 'Win Rate', breaks = seq(0, 13, 2), limits = c(0,13), expand = c(0,0)) +
  scale_fill_gradient('Win Rate', low = "lightgreen", high = "darkgreen") +
  theme(legend.position = 'right',
        panel.background = element_rect(fill = 'lightgray'),
        plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=8),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8))

####################################################################################################

### perform feature importance with boruta
library(Boruta)
set.seed(111)

boruta.newdf_train <- Boruta(result~., data = newdf, doTrace = 2)

# print the results
print(boruta.newdf_train)

#Boruta performed 99 iterations in 1.625165 hours.
#64 attributes confirmed important: BL, ClassRating, CP, D3, D1 and 59 more;
#9 attributes confirmed unimportant: FQ, FL2, FB, H2, H3 and 4 more;
#20 tentative attributes left: ALL, BP, W2, WW, F3 and 15 more;

final.boruta <- TentativeRoughFix(boruta.newdf_train)

print(final.boruta)

#Boruta performed 99 iterations in 1.625165 hours.
#Tentatives roughfixed over the last 99 iterations.
#71 attributes confirmed important: ALL, BQ, ClassRating, CP, D3 and 66 more;
#22 attributes confirmed unimportant: BP, W2, WW, F3, FS and 17 more;


#https://stackoverflow.com/questions/47342553/boruta-box-plots-in-r

# generateCol is needed by plot.Boruta
generateCol<-function(x,colCode,col,numShadow){
  #Checking arguments
  if(is.null(col) & length(colCode)!=4)
    stop('colCode should have 4 elements.');
  #Generating col
  if(is.null(col)){
    rep(colCode[4],length(x$finalDecision)+numShadow)->cc;
    cc[c(x$finalDecision=='Confirmed',rep(FALSE,numShadow))]<-colCode[1];
    cc[c(x$finalDecision=='Tentative',rep(FALSE,numShadow))]<-colCode[2];
    cc[c(x$finalDecision=='Rejected',rep(FALSE,numShadow))]<-colCode[3];
    col=cc;
  }
  return(col);
}


# Modified plot.Boruta
plot.Boruta.sel <- function(
    x,
    pars = NULL,
    colCode = c('green','yellow','red','blue'),
    sort = TRUE,
    whichShadow = c(TRUE, TRUE, TRUE),
    col = NULL, xlab = 'Attributes', ylab = 'Importance', ...) {
  
  #Checking arguments
  if(class(x)!='Boruta')
    stop('This function needs Boruta object as an argument.');
  if(is.null(x$ImpHistory))
    stop('Importance history was not stored during the Boruta run.');
  
  #Removal of -Infs and conversion to a list
  lz <- lapply(1:ncol(x$ImpHistory), function(i)
    x$ImpHistory[is.finite(x$ImpHistory[,i]),i]);
  colnames(x$ImpHistory)->names(lz);
  
  #Selection of shadow meta-attributes
  numShadow <- sum(whichShadow);
  lz <- lz[c(rep(TRUE,length(x$finalDecision)), whichShadow)];
  
  #Generating color vector
  col <- generateCol(x, colCode, col, numShadow);
  
  #Ordering boxes due to attribute median importance
  if (sort) {
    ii <- order(sapply(lz, stats::median));
    lz <- lz[ii];
    col <- col[ii];
  }
  
  # Select parameters of interest
  if (!is.null(pars)) lz <- lz[names(lz) %in% pars];
  
  #Final plotting
  graphics::boxplot(lz, xlab = xlab, ylab = ylab, col = 'green', cex.lab = 0.6, cex.axis=0.6);
  invisible(x);
}

#plot.Boruta.sel(final.boruta, pars = c('PRWIN', 'RS1', 'RS2', 'RS3', 'WGT', 'WINS', 'TRT', 'XPEAK', 'TBASE', 'ClassRating', 'SP_ODDS', 'TrainerWinP', 'JockeyWinP'))
plot.Boruta.sel(final.boruta, pars = c('JockRate', 'WT', 'W', 'TrainRate', 'OM', 'SH', 'OC', 'OL',
                                       'R1', 'R2', 'R3', 'R4', 'SHWIN'))

####################################################################################

# build the model
library(gbm)
library(xgboost)
library(caret)

# take only relevant cols
df3 <- df2[, c(2,18:30)]

# split into train and test data
#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(df3), replace=TRUE, prob=c(0.7,0.3))
train  <- df3[sample, ]
test   <- df3[!sample, ]

glm.fit.final <- glm(result ~.,family=binomial(link='logit'),data=train)

# predict values for test data
pred <- predict(glm.fit.final, test, type = "response")
pred <- ifelse(pred > 0.5,1,0)

sum(test$result)
# results
caret::RMSE(pred, test$result)
## [1] 0.278


