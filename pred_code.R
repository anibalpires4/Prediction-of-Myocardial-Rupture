#' ---
#' title: "Statistical Methods in Data Mining"
#' date: "November, 2022"
#' output: pdf_document
#' ---


#
# This block calls all the needed libraries
#

{
        library(rio)
        library(caTools)
        library(caret)
        library(klaR)
        library(MASS)
        library(psych)
        library(class)
        library(e1071)
        library(tidyr)
        library(dplyr)
        library(Hmisc)
        library(ROSE)
        library(ROCR)
        library(pROC)
        library(MLeval)
        library(xtable)
        library(GGally)
        library(corrplot)
        library(polycor)
        library(TSPred)
        library(naivebayes)
        library(descr)
        library(doParallel)
        library(viridis)
}

#
# This block is for parallel processing
#
# Start
#cl <- makeCluster(2)
#doParallel::registerDoParallel(cl)
# Stop
#stopCluster(cl)


#
# Here we define some helpful functions
#

mode <- function(x, na.rm = TRUE) {
        x <- as.integer(x)
        u <- unique(x)
        if(na.rm == TRUE) {u <- na.omit(u)}
        tab <- tabulate(match(x, u))
        u[tab == max(tab)]
}

colModes <- function(df) {
        apply(df, 2, mode)
}

colMedians <- function(df, na.rm = TRUE) {
        apply(df, 2, as.integer) %>%
                apply(., 2, median, na.rm = na.rm)
}

# This function helps print the plots to PDF
# with matching formatting
printToPDF <- function(plot, name) {
## Make sure that plot is set by a %>% operator

        scale <- 5
        pdf(file = name,
            onefile = TRUE,
            width = 1.618*scale,
            height = 1*scale)

        par(font.axis = 2, font.lab = 2, font = 1)

        plot

        dev.off()
}

# This function calls the relevant comparisons and plots
# to analyse the classifications methods
modRes <- function(model, data1, data2) {

        modName <- paste(deparse(substitute(model)),".pdf", sep = "")

        # Model information provided by caret
        print(model)
        print(plot(model))

        # Perform classification at each dataset
        class1 <- predict(model, newdata = data1)
        class2 <- predict(model, newdata = data2)

        # Confusion Matrix at each dataset
        cmat1 <- caret::confusionMatrix(data = class1, reference = data1$RAZRIV)
        cmat2 <- caret::confusionMatrix(data = class2, reference = data2$RAZRIV)

        print(cmat1)
        print(cmat2)

        # Create the ROC curves
        roc1 <- pROC::roc(as.numeric(data1$RAZRIV),
                          as.numeric(class1),
                          print.auc = FALSE, plot = FALSE)
        roc2 <- pROC::roc(as.numeric(data2$RAZRIV),
                          as.numeric(class2),
                          print.auc = FALSE, plot = FALSE)

        # Plot the ROC curve or save to PDF
        {
                plot(1-roc1$specificities,
                     roc1$sensitivities,
                     col = "red",
                     type = "l",
                     lty = 1,
                     lwd = 1.5,
                     main = "ROC curve",
                     xlab = "False Positive Rate",
                     ylab = "True Positive Rate")

                lines(1-roc2$specificities,
                      roc2$sensitivities,
                      col = "blue",
                      lty = 2,
                      lwd = 2.0)

                mtext(paste("AUC(Omitted) = ", round(roc1$auc, 2),
                            "  AUC(Imputed) = ", round(roc2$auc,2)),
                      side = 3)

                legend("topleft",
                       # By default we input the omitted dataset first
                       # and the Imputed dataset second
                       legend = c("Omitted", "Imputed"),
                       lty = c(1, 2),
                       col = c("red", "blue"))
        } #%>% printToPDF(. , name = modName)
        ## Un/comment at %>% to save to PDF or display
}


#
# Data importing
#

{
        ## Column names and types
        indices <- read.table("MI.indices", header = FALSE, sep = ";")

        ## Import data
        data_raw <- read.csv("MI.data", header = FALSE, na.strings = "?",
                             col.names = indices$V1,
                             colClasses = indices$V2)
}

# Variables types and domains for all data
barplot(sort(table(indices$V3), decreasing = TRUE),
        ylim = c(0,100),
        col = viridis(6),
        las = 1,
        cex.names = 0.75,
        main = "Distribution of the Types of Variables",
        xlab = "Types of Variables",
        ylab = "Number of Variables") #%>% printToPDF(. , name = "all_features.pdf")


#
# Remove unneeded columns
#

{
        data_raw <- subset(data_raw,
                           select = c(-1, -95, -102, -105, -113, -114, -115, -116, -117, -118, -120, -121, -122, -123, -124))
        indices <- data.frame(indices[c(-1, -95, -102, -105, -113, -114, -115, -116, -117, -118, -120, -121, -122, -123, -124) , ])
}

# Variable types and domains for relevant data
barplot(sort(table(indices$V3), decreasing = TRUE),
        ylim = c(0,100),
        col = viridis(6),
        las = 1,
        cex.names = 0.75,
        main = "Distribution of the Types of Variables",
        xlab = "Types of Variables",
        ylab = "Number of Variables") #%>% printToPDF(. , name = "relevant_features.pdf")


#
# Missing values
#

hist(colMeans(is.na(data_raw))*100,
     col = 1:100,
     breaks = c(0:100),
     main = "Missing Values by Feature",
     xlab = "Percentage of Missing Values",
     ylab = "Number of Features") #%>% printToPDF(. , name = "features_na.pdf")

hist(rowMeans(is.na(data_raw))*100,
     col = 1:100,
     breaks=c(0:100),
     main = "Missing Values by Entry",
     xlab = "Percentage of Missing Values",
     ylab = "Number of Entries") #%>% printToPDF(. , name = "entries_na.pdf")

# Total and Percentage of NAs
sum(is.na(data_raw)) # Total
sum(is.na(data_raw))*100/(nrow(data_raw)*ncol(data_raw)) # Percentage

# Percentage of NAs per feature
per_NA <- colMeans(is.na(data_raw))*100
per_NA

# Attributes with more than 20% missing values
cutoff <- 20
toDrop <- per_NA[which(per_NA >= cutoff)]
sort(toDrop, decreasing = TRUE)

# Contingency tables and Chi2 test between RAZRIV and each feature to drop
for(feat in names(toDrop)) {
        # Output .txt files for each pair table/test
        print.to.file <- FALSE

        # Do not make cross tables for numeric variables
        if(!indices[indices$V1 == feat, 2] == "numeric") {

                descr::CrossTable(data_raw$RAZRIV, data_raw[ , feat],
                                  dnn = c("RAZRIV", feat),
                                  missing.include = TRUE,
                                  fisher = TRUE,
                                  chisq = TRUE,
                                  expected = TRUE,
                                  prop.c = FALSE,
                                  prop.t = FALSE,
                                  prop.chisq = FALSE,
                                  sresid = TRUE,
                                  row.labels = TRUE,
                                  cell.layout = FALSE,
                                  format = "SPSS") %>%
                        {
                                if(print.to.file) {
                                        sink(file = paste(feat, ".txt", sep = ""),
                                             append = FALSE)
                                        print(.)
                                        sink()
                                }
                                else print(.)
                        }
        }
}


#
# Data preparation
#

# Remove attributes with missing values percentage >= cutoff
{
        df_miss <- data_raw[-which(per_NA >= cutoff)]
        indices_miss <- indices[-which(per_NA >= cutoff), ]
}

## New total missing values
sum(is.na(df_miss)) #Total
sum(is.na(df_miss))*100/(nrow(df_miss)*ncol(df_miss)) #Percentage

# RAZRIV outcomes / Balanced Design
table(df_miss$RAZRIV) # Totals
round(table(df_miss$RAZRIV)*100/nrow(df_miss),2) #Percentage

## Split the data set
{
        set.seed(2022)
        # Split ratio 70/30%
        spliter <- caTools::sample.split(df_miss, SplitRatio = .7)
        df_train <- subset(df_miss, spliter == TRUE)
        df_test <- subset(df_miss, spliter == FALSE)
}

### Create the datasets with deleted NAs
{
        ## No scaling
        df_train_omit <- na.omit(df_train)
        df_test_omit <- na.omit(df_test)

        ## Min-max scaling
        df_train_omit_minmax <- na.omit(df_train)
        df_test_omit_minmax <- na.omit(df_test)

        for(feat in indices_miss[indices_miss$V3 == "Real", 1]) {
                df_train_omit_minmax[feat] <- TSPred::minmax(df_train_omit_minmax[feat])
                df_test_omit_minmax[feat] <- TSPred::minmax(df_test_omit_minmax[feat])
        }
}

#### Check RAZRIV outcome ratios
table(df_train_omit$RAZRIV)*100/nrow(df_train_omit)
table(df_test_omit$RAZRIV)*100/nrow(df_test_omit)

table(df_train_omit_minmax$RAZRIV)*100/nrow(df_train_omit_minmax)
table(df_test_omit_minmax$RAZRIV)*100/nrow(df_test_omit_minmax)


### Dataset with imputed NAs
{
        means <- colMeans(df_train[indices_miss[indices_miss$V3 == "Real", 1]],
                          na.rm = TRUE) %>%
                round(.)

        medians <- colMedians(df_train[indices_miss[indices_miss$V3 == "Ordinal" | indices_miss$V3 == "Partially Ordered", 1]])

        modes <- colModes(df_train[indices_miss[indices_miss$V3 == "Nominal", 1]])
}

#### Create the training set
{
        ## No scaling
        df_train_imp <- df_train

        for(feat in indices_miss[indices_miss$V3 == "Real", 1]) {
                df_train_imp[feat][is.na(df_train_imp[feat])] <- means[feat]
        }

        for(feat in indices_miss[indices_miss$V3 == "Nominal", 1]) {
                df_train_imp[feat][is.na(df_train_imp[feat])] <- as.factor(modes[feat])
        }

        for(feat in indices_miss[indices_miss$V3 == "Ordinal" | indices_miss$V3 == "Partially Ordered", 1]) {
                df_train_imp[feat][is.na(df_train_imp[feat])] <- as.factor(medians[feat])
        }

        ## Min-max scaling
        df_train_imp_minmax <- df_train_imp

        for(feat in indices_miss[indices_miss$V3 == "Real", 1]) {
                df_train_imp_minmax[feat] <- TSPred::minmax(df_train_imp[feat])
        }
}

#### Check if all NAs were imputed and RAZRIV outcome ratio
sum(is.na(df_train_imp))
table(df_train_imp$RAZRIV)*100/nrow(df_train_imp)

sum(is.na(df_train_imp_minmax))
table(df_train_imp_minmax$RAZRIV)*100/nrow(df_train_imp_minmax)

#### Create the testing set
{
        ## No scaling
        df_test_imp <- df_test

        for(feat in indices_miss[indices_miss$V3 == "Real", 1]) {
                df_test_imp[feat][is.na(df_test_imp[feat])] <- means[feat]
                df_test_imp[feat] <- TSPred::minmax(df_test_imp[feat])
        }

        for(feat in indices_miss[indices_miss$V3 == "Nominal", 1]) {
                df_test_imp[feat][is.na(df_test_imp[feat])] <- as.factor(modes[feat])
        }

        for(feat in indices_miss[indices_miss$V3 == "Ordinal" | indices_miss$V3 == "Partially Ordered", 1]) {
                df_test_imp[feat][is.na(df_test_imp[feat])] <- as.factor(medians[feat])
        }

        ## Min-max scaling
        df_test_imp_minmax <- df_test_imp

        for(feat in indices_miss[indices_miss$V3 == "Real", 1]) {
                df_test_imp_minmax[feat] <- TSPred::minmax(df_test_imp[feat])
        }
}

#### Check if all NAs were imputed and RAZRIV outcome ratio
sum(is.na(df_test_imp))
table(df_test_imp$RAZRIV)*100/nrow(df_test_imp)

sum(is.na(df_test_imp_minmax))
table(df_test_imp_minmax$RAZRIV)*100/nrow(df_test_imp_minmax)


# Make the outcome factors compatible with caret::train function
{
        levels(df_train_omit$RAZRIV) <- c("X0", "X1")
        levels(df_train_omit_minmax$RAZRIV) <- c("X0", "X1")

        levels(df_test_omit$RAZRIV) <- c("X0", "X1")
        levels(df_test_omit_minmax$RAZRIV) <- c("X0", "X1")

        levels(df_train_imp$RAZRIV) <- c("X0", "X1")
        levels(df_train_imp_minmax$RAZRIV) <- c("X0", "X1")

        levels(df_test_imp$RAZRIV) <- c("X0", "X1")
        levels(df_test_imp_minmax$RAZRIV) <- c("X0", "X1")
}


#
# Memory clean up
#

# At this point some dataset, variables and functions are no longer needed
# Uncomment and run the following to free-up some memory
rm(data_raw, indices, df_miss, indices_miss, df_train, df_test, means, medians, modes, per_NA, print.to.file, spliter, toDrop, cutoff, feat, mode, colModes, colMedians)
gc()


#
# k-Nearest Neighbours
#

# This function is defined to help call caret::train
# with different parameters for kNN
knnClass <- function(data, sampling = NULL, kmax = 1:10, cv.number = 5, seed = 2022) {

        # Here we set the seed values for caret::train
        set.seed(seed)
        nSeeds <- cv.number
        nPars <- length(kmax)*2*4 # 2 distances, 4 kernels
        seeds <- vector(mode = "list", length = nSeeds+1)
        for(i in 1:nSeeds) seeds[[i]] <- sample.int(1000, nPars)
        seeds[[nSeeds+1]] <- sample.int(1000, 1)

        # Main classifier function
        caret::train(RAZRIV ~ .,
                     data       = data,
                     method     = "kknn",
                     tuneGrid   = expand.grid(kmax = kmax,
                                              distance = 1:2,
                                              kernel = c("rectangular",
                                                         "gaussian",
                                                         "rank",
                                                         "optimal")
                                              ),
                     trControl  = trainControl(method = "cv",
                                               number = cv.number,
                                               sampling = sampling,
                                               seeds = seeds,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary,
                                               savePredictions = TRUE),
                     metric     = "ROC"
                     )
}

### naming: mod_knn_<omit,imp>_<none,minmax>_<none,up,down>

# With NAs omitted

mod_knn_omit_none_none <- knnClass(data = df_train_omit, sampling = NULL, kmax = 1:20)
modRes(mod_knn_omit_none_none, df_test_omit, df_test_imp)

mod_knn_omit_minmax_none <- knnClass(data = df_train_omit_minmax, sampling = NULL, kmax = 1:20)
modRes(mod_knn_omit_minmax_none, df_test_omit_minmax, df_test_imp_minmax)

mod_knn_omit_none_up <- knnClass(data = df_train_omit, sampling = c("up"), kmax = 1:20)
modRes(mod_knn_omit_none_up, df_test_omit, df_test_imp)

mod_knn_omit_minmax_up <- knnClass(data = df_train_omit_minmax, sampling = c("up"), kmax = 1:20)
modRes(mod_knn_omit_minmax_up, df_test_omit_minmax, df_test_imp_minmax)

mod_knn_omit_none_down <- knnClass(data = df_train_omit, sampling = c("down"), kmax = 1:20)
modRes(mod_knn_omit_none_down, df_test_omit, df_test_imp)

mod_knn_omit_minmax_down <- knnClass(data = df_train_omit_minmax, sampling = c("down"), kmax = 1:20)
modRes(mod_knn_omit_minmax_down, df_test_omit_minmax, df_test_imp_minmax)


# With NAs imputed

mod_knn_imp_none_none <- knnClass(data = df_train_imp, sampling = NULL, kmax = 1:20)
modRes(mod_knn_imp_none_none, df_test_omit, df_test_imp)

mod_knn_imp_minmax_none <- knnClass(data = df_train_imp_minmax, sampling = NULL, kmax = 1:20)
modRes(mod_knn_imp_minmax_none, df_test_omit_minmax, df_test_imp_minmax)

mod_knn_imp_none_up <- knnClass(data = df_train_imp, sampling = c("up"), kmax = 1:20)
modRes(mod_knn_imp_none_up, df_test_omit, df_test_imp)

mod_knn_imp_minmax_up <- knnClass(data = df_train_imp_minmax, sampling = c("up"), kmax = 1:20)
modRes(mod_knn_imp_minmax_up, df_test_omit_minmax, df_test_imp_minmax)

mod_knn_imp_none_down <- knnClass(data = df_train_imp, sampling = c("down"), kmax = 1:20)
modRes(mod_knn_imp_none_down, df_test_omit, df_test_imp)

mod_knn_imp_minmax_down <- knnClass(data = df_train_imp_minmax, sampling = c("down"), kmax = 1:20)
modRes(mod_knn_imp_minmax_down, df_test_omit_minmax, df_test_imp_minmax)


#
# Naive Bayes
#

# This function is defined to help call caret::train
# with different parameters for NB
nbClass <- function(data, sampling = NULL, cv.number = 5, seed = 2022) {

        # Here we set the seed values for caret::train
        set.seed(seed)
        nSeeds <- cv.number
        nPars <- 3*5*2 # 3 laplace, 5 adjust, 2 usekernel
        seeds <- vector(mode = "list", length = nSeeds+1)
        for(i in 1:nSeeds) seeds[[i]] <- sample.int(1000, nPars)
        seeds[[nSeeds+1]] <- sample.int(1000, 1)

        # Main classifier function
        caret::train(RAZRIV ~ .,
                     data       = data,
                     method     = "naive_bayes",
                     tuneGrid   = expand.grid(laplace = c("0", "0.5", "1"),
                                              adjust = c("0.01", "0.05",
                                                         "0.1", "0.5", "1"),
                                              usekernel = c(TRUE, FALSE)),
                     trControl  = trainControl(method = "cv",
                                               number = cv.number,
                                               sampling = sampling,
                                               seeds = seeds,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary,
                                               savePredictions = TRUE),
                     metric     = "ROC"
                     )
}

### naming: mod_nb_<omit,imp>_<none,minmax>_<none,up,down>

# With NAs omitted

mod_nb_omit_none_none <- nbClass(data = df_train_omit, sampling = NULL)
modRes(mod_nb_omit_none_none, df_test_omit, df_test_imp)

mod_nb_omit_minmax_none <- nbClass(data = df_train_omit_minmax, sampling = NULL)
modRes(mod_nb_omit_minmax_none, df_test_omit_minmax, df_test_imp_minmax)

mod_nb_omit_none_up <- nbClass(data = df_train_omit, sampling = c("up"))
modRes(mod_nb_omit_none_up, df_test_omit, df_test_imp)

mod_nb_omit_minmax_up <- nbClass(data = df_train_omit_minmax, sampling = c("up"))
modRes(mod_nb_omit_minmax_up, df_test_omit_minmax, df_test_imp_minmax)

mod_nb_omit_none_down <- nbClass(data = df_train_omit, sampling = c("down"))
modRes(mod_nb_omit_none_down, df_test_omit, df_test_imp)

mod_nb_omit_minmax_down <- nbClass(data = df_train_omit_minmax, sampling = c("down"))
modRes(mod_nb_omit_minmax_down, df_test_omit_minmax, df_test_imp_minmax)


# With NAs imputed

mod_nb_imp_none_none <- nbClass(data = df_train_imp, sampling = NULL)
modRes(mod_nb_imp_none_none, df_test_omit, df_test_imp)

mod_nb_imp_minmax_none <- nbClass(data = df_train_imp_minmax, sampling = NULL)
modRes(mod_nb_imp_minmax_none, df_test_omit_minmax, df_test_imp_minmax)

mod_nb_imp_none_up <- nbClass(data = df_train_imp, sampling = c("up"))
modRes(mod_nb_imp_none_up, df_test_omit, df_test_imp)

mod_nb_imp_minmax_up <- nbClass(data = df_train_imp_minmax, sampling = c("up"))
modRes(mod_nb_imp_minmax_up, df_test_omit_minmax, df_test_imp_minmax)

mod_nb_imp_none_down <- nbClass(data = df_train_imp, sampling = c("down"))
modRes(mod_nb_imp_none_down, df_test_omit, df_test_imp)

mod_nb_imp_minmax_down <- nbClass(data = df_train_imp_minmax, sampling = c("down"))
modRes(mod_nb_imp_minmax_down, df_test_omit_minmax, df_test_imp_minmax)

