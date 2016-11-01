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


########  R packages ----------------

library(ggplot2)
library(lubridate)
library(ggfortify)
library(psych)
library(MASS)
library(nFactors)
library(tabplot)
library(plyr)

########  environment setting ----------------
# For RStudio only 

setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path)) 


####################################
####   abstracts wordcloud
###################################


# install.packages("PubMedWordcloud",dependencies=TRUE)

library("tm")
library("SnowballC")
library("PubMedWordcloud")
library("RColorBrewer")
library("ggplot2")

##### high quality ----------------


pmids=c("23360868", "17083854", "26915340", "26833102", "26919330", "27274756", "26700640","27087044", "25418531", "25561362", "26046458", "26280365", "26502807", "25039593", "25208594", "24606161", "24588075", "24284419", "24238156", "24124809", "23909714", "23772556", "23528816", "23797343", "23550658", "23600465", "22404103", "24151011", "22522793", "23208415", "23069736", "21862748", "21315483", "16437433")
abstracts=getAbstracts(pmids)
cleanAbs=cleanAbstracts(abstracts,stemDoc =TRUE)
plotWordCloud(cleanAbs,min.freq = 5, scale = c(5, 0.3))
bar_plot_high<-subset(cleanAbs, freq>20)
bar_plot_high$percent<-(bar_plot_high$freq/44)
bar_plot_high$quality<-(bar_plot_high$quality=c(rep("high_quality",44)))
p<-ggplot(bar_plot_high, aes(x=reorder(word, freq), y=freq)) +geom_bar(stat="identity")+coord_flip()

##########  low quality  ----------------


pmids_2=c("20443997", "22512681", "27386050", "26917347", "27062949", "19293497", "25677764", "26783352", "27061047", "26395500", "26600499", "25424331", "25604924", "25464252", "25697099", "25738846", "26713286", "25362715", "23425140", "25114684", "25039309", "24496885", "24890463", "24401219", "25052261", "24985560", "23711766", "23845148", "23845150", "23297015", "22812568", "22779910", "23696234", "23014338", "23442134", "23223703", "22278662", "22509259", "22512680", "22512682", "22512678", "22041254", "20927490", "20580412", "20546247", "20443996", "20043014", "19824740", "19664846", "17671883", "12639460", "11899141", "22182441", "24170986", "22967166")
abstracts_2=getAbstracts(pmids_2)
cleanAbs_2=cleanAbstracts(abstracts_2,stemDoc =TRUE)
plotWordCloud(cleanAbs_2,min.freq = 5, scale = c(5, 0.3))
bar_plot_low<-subset(cleanAbs_2, freq>20)
bar_plot_low$percent<-(bar_plot_low$freq/40)
bar_plot_low$quality<-(bar_plot_low$quality=c(rep("low_quality",40)))
bar_plot_low$percent<-(-bar_plot_low$percent)
p2<-ggplot(bar_plot_low, aes(x=reorder(word, freq), y=freq)) +geom_bar(stat="identity")+coord_flip()


##########  all plots  ----------------

high_vs_low_db<-rbind(bar_plot_high, bar_plot_low)
p3<-ggplot(high_vs_low_db, aes(x=reorder(word, percent), y=percent, fill=quality)) +geom_bar(stat="identity", position="identity")+scale_fill_manual(values=c("skyblue1", "coral1"))+ylab("frequency")+xlab("words in abstracts")+scale_y_continuous(limits=c(-8,8))+coord_flip()
p3

old.par <- par(mfrow=c(1, 2))
plotWordCloud(cleanAbs,min.freq = 10, scale = c(3, 0.8))
plotWordCloud(cleanAbs_2,min.freq = 10, scale = c(3, 0.8))
par(old.par)