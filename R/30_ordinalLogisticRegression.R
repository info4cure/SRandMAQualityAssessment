#' --- 
#' title: "Methodological appraisal of systematic reviews and meta-analyses on psoriasis: role of funding sources, conflict of interest and pressure to publish" 
#' author: "Juan Ruano" 
#' date: "30 Sep 2016" 
#' institutions: Department of Dermatology, IMIBIC/Reina Sofia University Hospital/University of Cordoba, Cordoba, Spain
#' analyse: 01_Statistical analyses: descriptive, PCA, heatmaps, and regression model
#' --- 
#' 
#' R version 3.3.1 (2016-06-21)
#' Platform: x86_64-apple-darwin13.4.0 (64-bit)
#' Running under: OS X 10.9.5 (Mavericks)



################  regression model, cross-validation test and probability plots  --------------------------
########  R packages ----------------

library(ggplot2)
library(lubridate)
library(ggfortify)
library(psych)
library(MASS)
library(nFactors)
library(tabplot)
library(plyr)

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)


library(rms)


########  read .csv files ----------------

file1<-read.csv2("AMSTARfinal_metadatos_articulos_con_topics.csv", 
                 sep = ";", 
                 dec= ".", 
                 stringsAsFactors = TRUE,
                 header = TRUE)
DB1<-as.data.frame(file1)


####  tiding dataset
DB1$Total.Cites                   <- as.numeric(DB1$Total.Cites)
DB1$Cited.Half.life               <- as.numeric(DB1$Cited.Half.life)
DB1$Citing.Half.life              <- as.numeric(DB1$Citing.Half.life)
DB1$Normalized.Eigenfactor        <- as.numeric(DB1$Normalized.Eigenfactor)
DB1$num_institutions              <- as.numeric(DB1$num_institutions)
DB1$conflict_of_interest          <- as.numeric(DB1$conflict_of_interest)

DB1$funding_academic              <- as.factor(DB1$funding_academic)
DB1$funding_industry              <- as.factor(DB1$funding_industry)
DB1$funding                       <- as.factor(DB1$funding)

DB1$metaanalysis_included         <- factor(DB1$metaanalysis_included)
DB1$AMSTAR_levels                 <- factor(DB1$AMSTAR_levels, levels=c("low_quality", "moderate_quality", "high_quality"))
DB1$AMSTAR_levels_2               <- factor(DB1$AMSTAR_levels_2, levels=c("low_quality", "moderate_quality", "high_quality"))
DB1$SJR.Best.Quartile             <- factor(DB1$SJR.Best.Quartile, levels=c("Q1", "Q2", "Q3", "Q4"))
DB1$Country                       <- factor(DB1$Country, levels=c("Brazil", "Canada", "China", "Germany", "India", "Ireland", "Netherlands", "Sweden", "Switzerland", "United Kingdom"))
DB1$topic                         <- factor(DB1$topic, levels=c("Comorbidities", "Economic analysis", "Pathogeny", "Treatment"))
DB1$year                          <- factor(DB1$year, levels=c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

levels(DB1$funding_industry)      <- c("No","Funded by Pharma")
levels(DB1$funding_academic)      <- c("No","Funded by Academic")
levels(DB1$metaanalysis_included) <- c("No","Metanalisis included")
levels(DB1$clinical_trials)       <- c("No","Analysis of clinical_trials")
levels(DB1$observational_studies) <- c("No","Analysis of observational studies")
levels(DB1$systematic_reviews)    <- c("No","Analysis of systematic reviews")
levels(DB1$economic_studies)      <- c("No","Analysis of economic studies")

DB1$topic_treatment               <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analysis", "Pathogeny", "Treatment"), to = c("0", "0", "0", "Treatment"))
DB1$topic_Pathogeny               <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analysis", "Pathogeny", "Treatment"), to = c("0", "0", "Pathogeny", "0"))
DB1$topic_Economic_analysis       <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analysis", "Pathogeny", "Treatment"), to = c("0", "Economic analysis", "0", "0"))
DB1$topic_Comorbidities           <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analysis", "Pathogeny", "Treatment"), to = c("Comorbidities", "0", "0", "0"))





######################################################## 
######## 1: Multinomial logistic regression    ########
########################################################

##########  ordinal regression dataset ---------

DB1_ordinal_regression_dataset    <-na.omit(DB1[,c("AMSTAR_levels_2",
                                                   "page_count", 
                                                   "conflict_of_interest",
                                                   "X5.Year.Impact.Factor",
                                                   "funding_academic",
                                                   "metaanalysis_included",
                                                   "Article.Influence.Score")])

##########  ordinal regression model ---------

model_ordinal_regression          <-orm(AMSTAR_levels_2 ~ log(page_count,2)+
                                          conflict_of_interest+
                                          X5.Year.Impact.Factor+
                                          funding_academic+
                                          metaanalysis_included,
                                          Article.Influence.Score,
                                          data = DB1_ordinal_regression_dataset)

########## ordinal regression results OR, 95%CI ---------


(ci <- confint(model_ordinal_regression))
exp(coef(model_ordinal_regression))
exp(cbind(OR = coef(model_ordinal_regression), ci))


############################################# 
######## 2: model Validation  ########
#############################################

# load the library
library(caret)
library(klaR)
library(e1071)

#################################### 
######### 1:Data Split ----------------

# defining an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(DB1_ordinal_regression_dataset$AMSTAR_levels_2, p=split, list=FALSE)
data_train <- DB1_ordinal_regression_dataset[ trainIndex,]
data_test <- DB1_ordinal_regression_dataset[-trainIndex,]

# training a naive bayes model
model <- NaiveBayes(AMSTAR_levels_2~page_count+conflict_of_interest+funding_academic+metaanalysis_included, data=DB1_ordinal_regression_dataset)

# making predictions
x_test <- data_test[,2:8]
y_test <- data_test[,1]
predictions <- predict(model, x_test)

# summarizing results
confusionMatrix(predictions$class, y_test)

# Results:
# Confusion Matrix and Statistics
# 
#                   Reference
# Prediction         low_quality moderate_quality high_quality
#   low_quality                5                4            1
#   moderate_quality           1               12            4
#   high_quality               0                0            0
# 
# Overall Statistics
#                                          
#                Accuracy : 0.6296         
#                  95% CI : (0.4237, 0.806)
#     No Information Rate : 0.5926         
#     P-Value [Acc > NIR] : 0.42726        
#                                          
#                   Kappa : 0.3199         
#  Mcnemar's Test P-Value : 0.07855        
# 
# Statistics by Class:
# 
#                      Class: low_quality Class: moderate_quality Class: high_quality
# Sensitivity                      0.8333                  0.7500              0.0000
# Specificity                      0.7619                  0.5455              1.0000
# Pos Pred Value                   0.5000                  0.7059                 NaN
# Neg Pred Value                   0.9412                  0.6000              0.8148
# Prevalence                       0.2222                  0.5926              0.1852
# Detection Rate                   0.1852                  0.4444              0.0000
# Detection Prevalence             0.3704                  0.6296              0.0000
# Balanced Accuracy                0.7976                  0.6477              0.5000
# 

#################################### 
######### 2:Bootstrap ----------------

# define training control
train_control <- trainControl(method="boot", number=100)

# train the model
model <- train(AMSTAR_levels_2~page_count+conflict_of_interest+funding_academic+metaanalysis_included, data=DB1_ordinal_regression_dataset, trControl=train_control, method="nb")

# summarize results
print(model)

# Results:
# Naive Bayes 
# 
# 145 samples
#   4 predictor
#   3 classes: 'low_quality', 'moderate_quality', 'high_quality' 
# 
# No pre-processing
# Resampling: Bootstrapped (100 reps) 
# Summary of sample sizes: 145, 145, 145, 145, 145, 145, ... 
# Resampling results across tuning parameters:
# 
#   usekernel  Accuracy   Kappa    
#   FALSE      0.5789428  0.2572229
#    TRUE      0.5383559  0.2256051



#################################### 
######### 3:Repeated k-fold Cross Validation ----------------


# define training control
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model
model <- train(AMSTAR_levels_2~page_count+conflict_of_interest+funding_academic+metaanalysis_included, data=DB1_ordinal_regression_dataset, trControl=train_control, method="nb")

# summarize results
print(model)

# Results:
# Naive Bayes 
# 145 samples
#   4 predictor
#   3 classes: 'low_quality', 'moderate_quality', 'high_quality' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 131, 131, 131, 131, 130, 130, ... 
# Resampling results across tuning parameters:
# 
#   usekernel  Accuracy   Kappa    
#   FALSE      0.6078571  0.2900667
#    TRUE      0.5491071  0.2404542


#################################### 
######### 4:Leave One Out Cross Validation ----------------

# define training control
train_control <- trainControl(method="LOOCV")

# train the model
model <- train(AMSTAR_levels_2~page_count+conflict_of_interest+funding_academic+metaanalysis_included, data=DB1_ordinal_regression_dataset, trControl=train_control, method="nb")

# summarize results
print(model)

# Results:
# Naive Bayes 
# 
# 145 samples
#   4 predictor
#   3 classes: 'low_quality', 'moderate_quality', 'high_quality' 
# 
# No pre-processing
# Resampling: Leave-One-Out Cross-Validation 
# Summary of sample sizes: 144, 144, 144, 144, 144, 144, ... 
# Resampling results across tuning parameters:
# 
#   usekernel  Accuracy   Kappa    
#   FALSE      0.6275862  0.3136997
#    TRUE      0.5448276  0.2273535


###########################################################
######## 3: ordinal regression probability plots   ########
###########################################################

library(nnet)
library(memisc)

##########   ---------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
   }
  }

###### META-ANALYSES INCLUDED
#### vs X5.Year.Impact.Factor 
DB1$AMSTAR_levels_2_new <- relevel(DB1$AMSTAR_levels_2, ref = "high_quality")

#########################
##1____##log(page_count,2) x metaanalyses_included
#########################
DB1$page_count_2<-log(DB1$page_count,2)
test <- multinom(AMSTAR_levels_2_new ~ page_count_2 + metaanalysis_included, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dpage_count <- data.frame(metaanalysis_included = rep(c("No", "Metanalisis included"), each = 15),page_count_2 = rep(c(1:10), 3))

## store the predicted probabilities for each value of ses and write
pp.dpage_count <- cbind(dpage_count, predict(test, newdata = dpage_count, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.dpage_count[, c(3,5,4)], pp.dpage_count$metaanalysis_included, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_1 <- melt(pp.dpage_count, id.vars = c("metaanalysis_included", "page_count_2"), value.name = "probability")
head(lpp_1) # view first few 

#########################
##2____##conflict_of_interest x metaanalyses_included
#########################
DB1$AMSTAR_levels_2_new <- relevel(DB1$AMSTAR_levels_2, ref = "high_quality")
test <- multinom(AMSTAR_levels_2_new ~ conflict_of_interest + metaanalysis_included, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dconflict_of_interest_2 <- data.frame(metaanalysis_included = rep(c("No", "Metanalisis included"), each = 24),conflict_of_interest = rep(c(0:15), 3))

## store the predicted probabilities for each value of ses and write
pp.dconflict_of_interest_2 <- cbind(dconflict_of_interest_2, predict(test, newdata = dconflict_of_interest_2, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.dconflict_of_interest_2[, c(3,5,4)], pp.dconflict_of_interest_2$funding_academic, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_2 <- melt(pp.dconflict_of_interest_2, id.vars = c("metaanalysis_included", "conflict_of_interest"), value.name = "probability")
head(lpp_2) # view first few 


#########################
##3____##Article.Influence.Score x metaanalyses_included
#########################
DB1$AMSTAR_levels_2_new <- relevel(DB1$AMSTAR_levels_2, ref = "high_quality")
test <- multinom(AMSTAR_levels_2_new ~ Article.Influence.Score + metaanalysis_included, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dArticle.Influence.Score <- data.frame(metaanalysis_included = rep(c("No", "Metanalisis included"), each = 24),Article.Influence.Score = rep(c(0:15), 3))

## store the predicted probabilities for each value of ses and write
pp.dArticle.Influence.Score <- cbind(dArticle.Influence.Score, predict(test, newdata = dArticle.Influence.Score, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.dArticle.Influence.Score[, c(3,5,4)], pp.dArticle.Influence.Score$metaanalysis_included, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_3 <- melt(pp.dArticle.Influence.Score, id.vars = c("metaanalysis_included", "Article.Influence.Score"), value.name = "probability")
head(lpp_3) # view first few 


#########################
##4____##X5.Year.Impact.Factor x metaanalyses_included
#########################
test <- multinom(AMSTAR_levels_2_new ~ X5.Year.Impact.Factor + metaanalysis_included, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dX5.Year.Impact.Factor <- data.frame(metaanalysis_included = rep(c("No", "Metanalisis included"), each = 45),X5.Year.Impact.Factor = rep(c(0.5:30), 3))

## store the predicted probabilities for each value of ses and write
pp.X5.Year.Impact.Factor <- cbind(dX5.Year.Impact.Factor, predict(test, newdata = dX5.Year.Impact.Factor, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.X5.Year.Impact.Factor[, c(3,5,4)], pp.X5.Year.Impact.Factor$metaanalysis_included, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_4 <- melt(pp.X5.Year.Impact.Factor, id.vars = c("metaanalysis_included", "X5.Year.Impact.Factor"), value.name = "probability")
head(lpp_4) # view first few 


#########################
##5____##log(page_count,2) x funding_academic
#########################
DB1$page_count_2<-log(DB1$page_count,2)
test <- multinom(AMSTAR_levels_2_new ~ page_count_2 + funding_academic, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dpage_count_2 <- data.frame(funding_academic = rep(c("No", "Funded by Academic"), each = 15),page_count_2 = rep(c(1:10), 3))

## store the predicted probabilities for each value of ses and write
pp.dpage_count_2 <- cbind(dpage_count_2, predict(test, newdata = dpage_count_2, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.dpage_count_2[, c(3,5,4)], pp.dpage_count_2$funding_academic, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_5 <- melt(pp.dpage_count_2, id.vars = c("funding_academic", "page_count_2"), value.name = "probability")
head(lpp_5) # view first few 

#########################
##6____##conflict_of_interest x funding_academic
#########################
DB1$AMSTAR_levels_2_new <- relevel(DB1$AMSTAR_levels_2, ref = "high_quality")
test <- multinom(AMSTAR_levels_2_new ~ conflict_of_interest + funding_academic, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dconflict_of_interest_new <- data.frame(funding_academic = rep(c("No", "Funded by Academic"), each = 24),conflict_of_interest = rep(c(0:15), 3))

## store the predicted probabilities for each value of ses and write
pp.dconflict_of_interest_new <- cbind(dconflict_of_interest_new, predict(test, newdata = dconflict_of_interest_new, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.dconflict_of_interest_new[, c(3,5,4)], pp.dconflict_of_interest_new$funding_academic, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_6 <- melt(pp.dconflict_of_interest_new, id.vars = c("funding_academic", "conflict_of_interest"), value.name = "probability")
head(lpp_6) # view first few 


#########################
##7____##Article.Influence.Score x funding_academic
#########################
DB1$AMSTAR_levels_2_new <- relevel(DB1$AMSTAR_levels_2, ref = "high_quality")
test <- multinom(AMSTAR_levels_2_new ~ Article.Influence.Score + funding_academic, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dArticle.Influence.Score_2 <- data.frame(funding_academic = rep(c("No", "Funded by Academic"), each = 24),Article.Influence.Score = rep(c(0:15), 3))

## store the predicted probabilities for each value of ses and write
pp.dArticle.Influence.Score_2 <- cbind(dArticle.Influence.Score_2, predict(test, newdata = dArticle.Influence.Score_2, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.dArticle.Influence.Score_2[, c(3,5,4)], pp.dArticle.Influence.Score_2$funding_academic, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_7 <- melt(pp.dArticle.Influence.Score_2, id.vars = c("funding_academic", "Article.Influence.Score"), value.name = "probability")
head(lpp_7) # view first few 


#########################
##8____##X5.Year.Impact.Factor x funding_academic
#########################
test <- multinom(AMSTAR_levels_2_new ~ X5.Year.Impact.Factor + funding_academic, data = DB1)
summary(test)
z<-summary(test)$coefficients/summary(test)$standard.errors
#2-tailed z test
p<-(1-pnorm(abs(z), 0, 1))*2
## extract the coefficients from the model and exponentiate
exp(coef(test))
head(pp <- fitted(test))

#Another way to understand the model using the predicted probabilities is to look at the 
#averaged predicted probabilities for different values of the continuous predictor variable conflict_of_interest within each level of funding_industry

dX5.Year.Impact.Factor_2 <- data.frame(funding_academic = rep(c("No", "Funded by Academic"), each = 45),X5.Year.Impact.Factor = rep(c(0.5:30), 3))

## store the predicted probabilities for each value of ses and write
pp.dX5.Year.Impact.Factor_2 <- cbind(dX5.Year.Impact.Factor_2, predict(test, newdata = dX5.Year.Impact.Factor_2, type = "probs", se = TRUE))

## calculate the mean probabilities within each level of ses
by(pp.dX5.Year.Impact.Factor_2[, c(3,5,4)], pp.dX5.Year.Impact.Factor_2$funding_academic, colMeans)

##Sometimes, a couple of plots can convey a good deal amount of information. 
#Using the predictions we generated for the pp.Journal.Impact.Factor object above, 
#we can plot the predicted probabilities against the AMSTAR_levels_2_new score by the level of funding_industry 
#for different levels of the outcome variable.
## melt data set to long for ggplot2
lpp_8 <- melt(pp.dX5.Year.Impact.Factor_2, id.vars = c("funding_academic", "X5.Year.Impact.Factor"), value.name = "probability")
head(lpp_8) # view first few


lpp_1$variable<-factor(lpp_1$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_1$variable<-relabel(lpp_1$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

lpp_2$variable<-factor(lpp_2$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_2$variable<-relabel(lpp_2$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

lpp_3$variable<-factor(lpp_3$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_3$variable<-relabel(lpp_3$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

lpp_4$variable<-factor(lpp_4$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_4$variable<-relabel(lpp_4$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

lpp_5$variable<-factor(lpp_5$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_5$variable<-relabel(lpp_5$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

lpp_6$variable<-factor(lpp_6$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_6$variable<-relabel(lpp_6$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

lpp_7$variable<-factor(lpp_7$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_7$variable<-relabel(lpp_7$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

lpp_8$variable<-factor(lpp_8$variable, levels=c("high_quality", "moderate_quality","low_quality"))
lpp_8$variable<-relabel(lpp_8$variable, "high_quality"="High","moderate_quality"="Moderate","low_quality"="Low")

p1<-ggplot(lpp_1,aes(x = page_count_2, y = probability, colour = metaanalysis_included)) +
         geom_line(aes(linetype = metaanalysis_included, color = metaanalysis_included)) +
         scale_linetype_manual(values = c("solid", "dotted")) +
         scale_color_manual(values = c('#E69F00', '#E69F00')) + 
         scale_size_manual(values =c(3, 5)) + 
         facet_grid(variable ~ ., scales = "fixed") + 
         theme(legend.position = "top") + theme(legend.title = element_blank()) +
         theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted")) +
         theme(panel.grid.major = element_line(colour = "grey")) + 
         labs(x = "log2(page count)") +
         scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.20))

p2 <-ggplot(lpp_2,aes(x = conflict_of_interest, y = probability, colour = metaanalysis_included)) +
         geom_line(aes(linetype = metaanalysis_included, color = metaanalysis_included)) +
         scale_linetype_manual(values = c("solid", "dotted")) + scale_color_manual(values =c('#458B74', '#458B74')) + 
         scale_size_manual(values = c(3, 5)) +
         facet_grid(variable ~ ., scales ="fixed") + 
         theme(legend.position = "top") + theme(legend.title = element_blank()) +
         theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted")) +
         theme(panel.grid.major = element_line(colour = "grey")) + 
         labs(x = "Authors with conflict of interest") +
         scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.20))

p3 <-ggplot(lpp_3, aes(x = Article.Influence.Score, y = probability, colour = metaanalysis_included)) +
        geom_line(aes(linetype=metaanalysis_included, color=metaanalysis_included))+
        scale_linetype_manual(values=c("solid", "dotted"))+
        scale_color_manual(values=c('#6495ED','#6495ED'))+
        scale_size_manual(values=c(3, 5))+
        facet_grid(variable ~ ., scales="fixed")+
        theme(legend.position="top")+
        theme(legend.title=element_blank())+
        theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))+
        theme(panel.grid.major = element_line(colour = "grey"))+
        labs(x="Article Influence Score")+scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.20))

p4 <-ggplot(lpp_4, aes(x = X5.Year.Impact.Factor, y = probability, colour = metaanalysis_included)) +
        geom_line(aes(linetype=metaanalysis_included, color=metaanalysis_included))+
        scale_linetype_manual(values=c("solid", "dotted"))+
        scale_color_manual(values=c('#A2CD5A','#A2CD5A'))+
        scale_size_manual(values=c(3, 5))+
        facet_grid(variable ~ ., scales="fixed")+
        theme(legend.position="top")+
        theme(legend.title=element_blank())+
        theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))+
        theme(panel.grid.major = element_line(colour = "grey"))+
        labs(x="5 Year Impact Factor")+
        scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.20))

p5 <-ggplot(lpp_5, aes(x = page_count_2, y = probability, colour = funding_academic)) +geom_line(aes(linetype=funding_academic, color=funding_academic))+scale_linetype_manual(values=c("solid", "dotted"))+scale_color_manual(values=c('#BB0000','#BB0000'))+scale_size_manual(values=c(3, 5))+facet_grid(variable ~ .,scales="fixed")+theme(legend.position="top")+theme(legend.title=element_blank())+theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))+theme(panel.grid.major = element_line(colour = "grey"))+labs(x="log2(page count)")+scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.20))

p6 <-ggplot(lpp_6, aes(x = conflict_of_interest, y = probability, colour = funding_academic)) +geom_line(aes(linetype=funding_academic, color=funding_academic))+scale_linetype_manual(values=c("solid", "dotted"))+scale_color_manual(values=c('#006400','#006400'))+scale_size_manual(values=c(3, 5))+facet_grid(variable ~ ., scales="fixed")+theme(legend.position="top")+theme(legend.title=element_blank())+theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))+theme(panel.grid.major = element_line(colour = "grey"))+labs(x="Authors with conflict of interest")+scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.20))

p7 <-ggplot(lpp_7, aes(x = Article.Influence.Score, y = probability, colour = funding_academic)) +geom_line(aes(linetype=funding_academic, color=funding_academic))+scale_linetype_manual(values=c("solid", "dotted"))+scale_color_manual(values=c('#00009B','#00009B'))+scale_size_manual(values=c(3, 5))+facet_grid(variable ~ ., scales="fixed")+theme(legend.position="top")+theme(legend.title=element_blank())+theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))+theme(panel.grid.major = element_line(colour = "grey"))+labs(x="Article Influence Score")+scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.20))

p8 <-ggplot(lpp_8, aes(x = X5.Year.Impact.Factor, y = probability, colour = funding_academic)) +geom_line(aes(linetype=funding_academic, color=funding_academic))+scale_linetype_manual(values=c("solid", "dotted"))+scale_color_manual(values=c('#006400','#006400'))+scale_size_manual(values=c(3, 5))+facet_grid(variable ~ ., scales="fixed")+theme(legend.position="top")+theme(legend.title=element_blank())+theme(panel.grid.minor = element_line(colour = "grey", linetype = "dotted"))+theme(panel.grid.major = element_line(colour = "grey"))+labs(x="5 Year Impact Factor")+scale_y_continuous(limits=c(0,1),breaks = seq(0, 1, by = 0.20))


multiplot(p1,p5,p2,p6,p3,p7,p4,p8,cols=4)
