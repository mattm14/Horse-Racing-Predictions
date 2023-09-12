
# Predicting Outcomes in Horse Racing using Linear Regression Models

This project attempts to use predictive modelling as a means to predict outcomes in horse racing using linear regression models based on past data with some analyses. 

## Prerequisites
- plyr
- dplyr
- data.table
- tidyr
- tidyverse
- gbm
- xgboost
- caret

## Workflow
- Import the data  
- Pre-process the data by adding some key columns and removing some data that won't be required  
- Exploring and visualizing the data – win rate by distance and jockey/trainer win rates  
- Feature Engineering – extract features based on their importance  
- Build the prediction model – split data to train and test, build model by regression, select mean absolute error (MAE) as error metric

## Import Data
    # load required libraries
    library(plyr)
    library(dplyr)
    library(data.table)
    library(tidyr)
    library(tidyverse)

    # load the current files
    RaceData <- fread('/Data/RaceData.csv')
    HorseData <- fread('/Data/HorseData.csv')
    PastPerf <- fread('/Data/PastPerf.csv')
    JockeySR <- fread('/Data/Jockey_Stats.csv')
    TrainerSR <- fread('/Data/Trainer_Stats.csv', header = T)

## Data Pre-Processing

    # Combine files
    h = inner_join(PastPerf, HorseData, by = c("HORSEID"))
    h = inner_join(h, RaceData, by = c("RACEID"))

We will extract class ratings and will do that using the following function:

    # function to get class ratings
    f1<- function(x){
    str_match(x, "(\\*)(.*?)(\\*)" )[3]
    }

    # extract class ratings
    a <- dim(h)[1]
    values1 <- lapply(h$RACENAME, f1  )
    ClassRating1 <-data.frame(matrix(unlist(values1), nrow=a, byrow=T))
    names(ClassRating1) <- "ClassRating"
    h <- cbind.data.frame(h, ClassRating1)
    h$ClassRating <- as.numeric(as.character(h$ClassRating))

Next, we will add both jockey and trainer ratings:

    #add Jockey SR
    h$JockeyWinP <- JockeySR$WinP[match(h$JOCKEY, Jockey$JOCKEY)]    
    #add Jockey Rating
    h$JockeyRATE <- JockeySR$SRATE[match(h$JOCKEY, JockeySR$JOCKEY)]
    
    #add Trainer SR
    h$TrainerWinP <- TrainerSR$WinP[match(h$TRAINER, TrainerSR$TRAINER)]    
    #add Trainer Rating
    h$TrainerRATE <- TrainerSR$SRATE[match(h$TRAINER, TrainerSR$TRAINER)]

Some jockeys and trainers don't have ratings. We will replace these with the median ratings:

    h$JockeyWinP[is.na(h$JockeyWinP)] <- median(h$JockeyWinP, na.rm=TRUE)
    h$JockeyRATE[is.na(h$JockeyRATE)] <- median(h$JockeyRATE, na.rm=TRUE)

    h$TrainerWinP[is.na(h$TrainerWinP)] <- median(h$TrainerWinP, na.rm=TRUE)
    h$TrainerRATE[is.na(h$TrainerRATE)] <- median(h$TrainerRATE, na.rm=TRUE)

Maidens (races with horses with 0 wins) can be misleading, so let's remove them:

    ClassType <- c('Maiden', 'Maiden Hurdle', 'Maiden & Jumpers', 'Maiden Steeple')
    h <- subset(h, !(CLASS %in% ClassType))

## Explore and Visualize the data

Let's take a look at win rates by both sprint and staying distances.

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

![win_rate_sprint](https://github.com/mattm14/Horse-Racing-Predictions/assets/34406190/a2763a46-b94c-4fa2-b161-e4fd75a09605)

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

![win_rate_stay](https://github.com/mattm14/Horse-Racing-Predictions/assets/34406190/7b3c64fb-1021-4570-979b-bc879e3b360e)

This demonstrates that there is a strong correlation between the horse’s barrier position and its win rate, specifically over sprint distances (usually up to 1200m). Specifically, win rates are the highest starting from barrier position 1. Inside barrier positions perform the best, then as the barrier position gets wider, wins begin to drop off.

However, when exploring a horse’s win rate by barrier position over staying distances (typically greater than 2400m), there does not appear to be a strong correlation between the horse’s barrier position and its win rate. 

Therefore, the suggestion is that a horse’s barrier position over sprint distances is of greater importance than over staying distances.

Let's take a look at jockey and trainer win rates.

    # jockey rates
    ggplot(data = h, aes(x = factor(jockey, level = level_order), y = adj_win_rate, fill = adj_win_rate)) +
    geom_bar(width = 0.8, stat = "identity") +
    ggtitle('Jockey v Adj Win Rates - Season 2022/23') +
    scale_y_continuous(name = 'Adj Win Rate', breaks = seq(0, 25, 10), limits = c(0,25), expand = c(0,0)) +
    scale_fill_gradient('Adj Win Rate', low = "lightblue", high = "darkblue") +
    theme(legend.position = 'right',
            panel.background = element_rect(fill = 'lightgray'),
            plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
            legend.title=element_text(size=8), 
            legend.text=element_text(size=8),
            axis.text = element_text(size = 6),
            axis.title = element_text(size = 8)) + 
    labs(x = 'Jockey') +
    coord_flip()

![jock_win_rate](https://github.com/mattm14/Horse-Racing-Predictions/assets/34406190/562494f2-f860-4d80-8b22-38b43e427b00)

    # trainer rates
    ggplot(data = tnr, aes(x = factor(trainer, level = level_order), y = adj_win_rate, fill = adj_win_rate)) +
    geom_bar(width = 0.8, stat = "identity") +
    ggtitle('Trainer v Adj Win Rates - Season 2022/23') +
    scale_y_continuous(name = 'Adj Win Rate', breaks = seq(0, 25, 10), limits = c(0,25), expand = c(0,0)) +
    scale_fill_gradient('Adj Win Rate', low = "lightgreen", high = "darkgreen") +
    theme(legend.position = 'right',
            panel.background = element_rect(fill = 'lightgray'),
            plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
            legend.title=element_text(size=8), 
            legend.text=element_text(size=8),
            axis.text = element_text(size = 6),
            axis.title = element_text(size = 8)) + 
    labs(x = 'Trainer') +
    coord_flip()

![trainer_win_rate](https://github.com/mattm14/Horse-Racing-Predictions/assets/34406190/0c3a5ba5-0912-406e-8683-a7768df9c978)

The win rate of each jockey and trainer varies significantly. The win rate shown for each jockey and trainer is adjusted by a number of factors. This suggests that, depending on the jockey and/or trainer, they may have predictive power on the outcome of a race, hence they are included in the model.

## Perform feature importance with boruta

    library(Boruta)
    set.seed(111)
    
    # get importance using boruta
    boruta.newdf_train <- Boruta(result~., data = newDF, doTrace = 2)

Let's take a look at our results:

    # print the results
    print(boruta.newdf_train)

    #Boruta performed 99 iterations in 1.625165 hours.
    #64 attributes confirmed important: BL, ClassRating, CP, D3, D1 and 59 more;
    #9 attributes confirmed unimportant: FQ, FL2, FB, H2, H3 and 4 more;
    #20 tentative attributes left: ALL, BP, W2, WW, F3 and 15 more;

We'll perform a tenative rough fix on the remaining tentative attributes:

    final.boruta <- TentativeRoughFix(boruta.newdf_train)

    print(final.boruta)

    #Boruta performed 99 iterations in 1.625165 hours.
    #Tentatives roughfixed over the last 99 iterations.
    #71 attributes confirmed important: ALL, BQ, ClassRating, CP, D3 and 66 more;
    #22 attributes confirmed unimportant: BP, W2, WW, F3, FS and 17 more;

Let's plot these lot the results showing their relative importance. This can be done using the suggestion [here](https://stackoverflow.com/questions/47342553/boruta-box-plots-in-r).

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
    graphics::boxplot(lz, xlab = xlab, ylab = ylab, col = 'green', cex.lab = 0.5, cex.axis=0.5);
    invisible(x);
    }

We'll now plot the data using the most important features:  

    plot.Boruta.sel(final.boruta, pars = c('JockRate', 'WT', 'W', 'TrainRate', 'OM', 'SH', 'OC', 'OL',
                                       'R1', 'R2', 'R3', 'R4', 'SHWIN'))

![Horse_boruta](https://github.com/mattm14/Horse-Racing-Predictions/assets/34406190/fa8109ea-4b77-4aae-a5c6-07dd3e6bd299)

## Build the prediction model

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

    # results
    caret::RMSE(pred, test$result)
    ## [1] 0.278    
