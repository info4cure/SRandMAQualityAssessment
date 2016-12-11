#' --- 
#' title: "Methodological appraisal of systematic reviews and meta-analyses on psoriasis: role of funding sources, conflict of interest and pressure to publish" 
#' author: "Juan Ruano" 
#' date: "30 Sep 2016" 
#' institutions: Department of Dermatology, IMIBIC/Reina Sofia University Hospital/University of Cordoba, Cordoba, Spain
#' analyse: 01_Bubble plot pharma funding  vs  AMSTAR score
#' --- 
#' 
#' R version 3.3.1 (2016-06-21)
#' Platform: x86_64-apple-darwin13.4.0 (64-bit)
#' Running under: OS X 10.9.5 (Mavericks)


########  R packages ----------------

library(plyr)
library(ggplot2)
library(cowplot)

########  environment setting ----------------
# For RStudio only 

setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path)) 


######## Read .csv files --------------------------------

####  main DB -----

file1<-read.csv2("AMSTARfinal_metadatos_articulos_con_topics.csv", 
                 sep = ";", 
                 dec= ".", 
                 stringsAsFactors = TRUE,
                 header = TRUE)
DB1<-as.data.frame(file1)

####  scopus data -----

file5<-read.csv2("authors_metadata_SCOPUS.csv", 
                 sep = ";", 
                 dec= ".", 
                 stringsAsFactors = TRUE,
                 header = TRUE)
DB_scopus<-as.data.frame(file5)

########  Tiding dataset --------------------------------

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
DB1$topic                         <- factor(DB1$topic, levels=c("Comorbidities", "Economic analyses", "Pathogeny", "Treatment"))
DB1$year                          <- factor(DB1$year, levels=c("1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016"))

levels(DB1$funding_industry)      <- c("No","Funded by Pharma")
levels(DB1$funding_academic)      <- c("No","Funded by Academic")
levels(DB1$metaanalysis_included) <- c("No","Metanalisis included")
levels(DB1$clinical_trials)       <- c("No","Analyses of clinical_trials")
levels(DB1$observational_studies) <- c("No","Analyses of observational studies")
levels(DB1$systematic_reviews)    <- c("No","Analyses of systematic reviews")
levels(DB1$economic_studies)      <- c("No","Analyses of economic studies")
levels(DB1$AMSTAR_levels_2)       <- c("low quality", "moderate quality", "high quality")
levels(DB1$AMSTAR_levels_2)       <- factor(DB1$AMSTAR_levels_2, levels=c("low quality", "moderate quality", "high quality"))



DB1$topic_treatment               <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analyses", "Pathogeny", "Treatment"), to = c("0", "0", "0", "Treatment"))
DB1$topic_Pathogeny               <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analyses", "Pathogeny", "Treatment"), to = c("0", "0", "Pathogeny", "0"))
DB1$topic_Economic_analysis       <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analyses", "Pathogeny", "Treatment"), to = c("0", "Economic analyses", "0", "0"))
DB1$topic_Comorbidities           <- mapvalues(DB1$topic, from = c("Comorbidities", "Economic analyses", "Pathogeny", "Treatment"), to = c("Comorbidities", "0", "0", "0"))



DB_bubble<-merge(DB1, DB_scopus, by.x="article_Id", by.y="article_Id")


##########  bubble plot ----------------

DB_bubble$pharma_name<-factor(DB_bubble$pharma_name, levels=c("Galderma", "Pfizer", "Novartis", "AbbVie", "LEO Pharma", "Boots Healthcare International", "Janssen-Cilag", "Wyeth Pharma", "UCB Pharma", "MSD"))
ggplot(na.omit(DB_bubble[,c("AMSTAR_consensus_2","pharma_name")]), aes(x = AMSTAR_consensus_2, y =pharma_name)) +
  geom_count(colour="royalblue4")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11))+
  theme_minimal()+
  theme(legend.position="none")+
  theme(text = element_text(color = "gray10"),panel.grid.major = element_line(color = "gray80", size = 0.5),panel.grid.major.x = element_blank())+
  xlab("AMSTAR score")+ 
  ylab("Pharmaceutical Companies")+
  geom_vline(xintercept=c(4,8), linetype="dotted", colour = "blue", size = 1, alpha = .4)+
  scale_size(range = c(5,30))

##########  funding pharma rank ----------------

summary(DB_bubble$pharma_name)

##########  top institutions ranked by number of papers and AMSTAR score ----------------

count_authors<-unique(count(DB_bubble$Institution))
write.table(count_authors, file="count_authors.xls")


median_per_institutio<-unique(ddply(DB_bubble, .(Institution), summarize,  Rate1=median(AMSTAR_consensus_2)))
write.table(median_per_institutio, file="median_per_institutio.xls")


