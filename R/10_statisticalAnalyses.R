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


######## Read .csv files --------------------------------

file1<-read.csv2("AMSTARfinal_metadatos_articulos_con_topics.csv", 
                 sep = ";", 
                 dec= ".", 
                 stringsAsFactors = TRUE,
                 header = TRUE)
DB1<-as.data.frame(file1)


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













######## plotting heat maps --------------------------------
####  R packages ----------------

source("https://bioconductor.org/biocLite.R")
biocLite()
biocLite("ComplexHeatmap")
library(ComplexHeatmap)
library(circlize)
library(dendsort)
library(dendextend)


#### prepare matrix  ----------------
DB_heat                          <- na.omit(DB1[c(3,26,69,66,63,62,72,73,75,77,76,31,39,33,23,24,7,19:22, 56,57)])
DB_heat                          <- DB_heat[order(DB_heat$sort_AMSTAR_levels),]
heatDB                           <- as.matrix(DB_heat[c(1,18:21)])

topic_df                         <- (DB_heat[c(1,2)])
topic                            <- as.matrix(topic_df)

number_authors_df                <- (DB_heat[c(1,22)])
number_authors                   <- as.matrix(number_authors_df)

number_institutions_df           <- (DB_heat[c(1,23)])
number_institutions              <- as.matrix(number_institutions_df)

article_page_count_df            <- (DB_heat[c(1,3)])
article_page_count               <- as.matrix(article_page_count_df)

conflict_of_interest_df          <- (DB_heat[c(1,4)])
conflict_of_interest             <- as.matrix(conflict_of_interest_df)

funding_industry_df              <- (DB_heat[c(1,5)])
funding_industry                 <- as.matrix(funding_industry_df)

funding_academic_df              <- (DB_heat[c(1,6)])
funding_academic                 <- as.matrix(funding_academic_df)

clinical_trials_df               <- (DB_heat[c(1,7)])
clinical_trials                  <- as.matrix(clinical_trials_df)

observational_studies_df         <- (DB_heat[c(1,8)])
observational_studies            <- as.matrix(observational_studies_df)

systematic_reviews_df            <- (DB_heat[c(1,9)])
systematic_reviews               <- as.matrix(systematic_reviews_df)

metaanalyses_included_df         <- (DB_heat[c(1,10)])
metaanalyses_included            <- as.matrix(metaanalyses_included_df)

economic_studies_df              <- (DB_heat[c(1,11)])
economic_studies                 <- as.matrix(economic_studies_df)

journal_impact_factor_df         <- (DB_heat[c(1,12)])
journal_impact_factor            <- as.matrix(journal_impact_factor_df)

article_influence_score_df       <- (DB_heat[c(1,13)])
article_influence_score          <- as.matrix(article_influence_score_df)

five_year_impact_factor_df       <- (DB_heat[c(1,14)])
five_year_impact_factor          <- as.matrix(five_year_impact_factor_df)

google_cytes_df                  <- (DB_heat[c(1,15)])
google_cytes                     <- as.matrix(google_cytes_df)

web_of_science_df                  <- (DB_heat[c(1,16)])
web_of_science                     <- as.matrix(web_of_science_df)

type_df                          <- (DB_heat[c(1,17)])
type                             <- as.matrix(type_df)


#### individual heat maps ----------------

topicAreaPlot                    <- Heatmap(topic[,2], 
                                            name="Topic area of research", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = TRUE,
                                            col = c("navyblue","lightgreen", "chocolate4", "darkolivegreen"))

numAuthorsPlot                   <- Heatmap(number_authors[,2], 
                                            name="Number of authors", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            show_heatmap_legend = FALSE,
                                            c("white", "steelblue4"))

numInstitutionsPlot              <- Heatmap(number_institutions[,2], 
                                            name="Number of institutions", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            show_heatmap_legend = FALSE,
                                            c("white", "royalblue1"))

articlePageCountPlot             <- Heatmap(log(article_page_count[,2],2), 
                                            name="Article page count (log2)", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = colorRamp2(c(2, 8), 
                                            c("white", "forestgreen")))

numAuthorsConflictInterestPlot   <- Heatmap(conflict_of_interest[,2], 
                                            name="Number of authors with conflict of interest", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            #col = colorRamp2(c(0, 15), 
                                            show_heatmap_legend = FALSE,
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            c("white", "aquamarine4"))

fundingPharmaPlot                <- Heatmap(funding_industry[,2], 
                                            name="Funding by pharmaceutical companies", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = c("aquamarine3", "white"))

fundingAcademicPlot              <- Heatmap(funding_academic[,2], 
                                            name="Funding by academic institutions", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = c("firebrick1", "white"))

clinicalTrialsPlot               <- Heatmap(clinical_trials[,2], 
                                            name="Clinical trials analyses", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = c("white", "gray39"))

observationalStudiesPlot         <- Heatmap(observational_studies[,2], 
                                            name="Observational studies analyses", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = c("white", "seashell4"))

srPlot                           <- Heatmap(systematic_reviews[,2], 
                                            name="Systematic reviews analyses", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = c("white", "cadetblue4"))

metaanalysesIncludedPlot         <- Heatmap(metaanalyses_included[,2], 
                                            name="Meta-analyses included", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = c("cadetblue3", "white"))

economicStudiesPlot              <- Heatmap(economic_studies[,2], 
                                            name="Economic analyses included", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = FALSE,
                                            col = c("white", "darkorange1"))

journalImpactFactorPlot          <- Heatmap(log(journal_impact_factor[,2], 2), 
                                            name="Journal impact factor (log2)", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            #col = colorRamp2(c(-3, 4), 
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            show_heatmap_legend = FALSE,
                                            c("white", "tan4"))

articleInfluenceScorePlot        <- Heatmap(log(article_influence_score[,2],2), 
                                            name="Article influence score (log2)", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            #col = colorRamp2(c(-3, 4), 
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            show_heatmap_legend = FALSE,
                                            c("white", "tan3"))

fiveYearImpactFactorPlot         <- Heatmap(log(five_year_impact_factor[,2],2), 
                                            name="5-year impact factor (log2)", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            #col = colorRamp2(c(-0.3, 5), 
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            show_heatmap_legend = FALSE,
                                            c("white", "tan1"))

citesInGoogleScholarPlot         <- Heatmap(log(google_cytes[,2]+0.1,2), 
                                            name="Cites in Google Scholar (log2)", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            #col = colorRamp2(c(0, 100), 
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            show_heatmap_legend = FALSE,
                                            c("white", "orange"))

citesInWoSPlot                   <- Heatmap(log(web_of_science[,2]+0.1,2), 
                                            name="Cites in Web of Science (log2)", 
                                            show_row_names = FALSE, 
                                            cluster_rows = FALSE, 
                                            width = unit(5, "mm"), 
                                            #col = colorRamp2(c(0, 100), 
                                            heatmap_legend_param = list(color_bar = "continuous"),
                                            show_heatmap_legend = FALSE,
                                            c("white", "coral"))

amstarLevelsPlot                 <- Heatmap(type[,2], 
                                            name="AMSTAR levels", 
                                            show_row_names = FALSE, 
                                            width = unit(5, "mm"), 
                                            show_heatmap_legend = TRUE,
                                            col = c("steelblue3", "red", "lightgreen"))

dend <- hclust(dist(heatDB[,2:5]))
dend <- color_branches(dend, k = 6)

pcaFactors                       <- Heatmap(heatDB[,2:5], 
                                            cluster_columns = FALSE, 
                                            cluster_rows = TRUE, 
                                            show_row_names = FALSE, 
                                            width = unit(20, "mm"),
                                            km = 6,
                                            row_order = 4:220,
                                            show_heatmap_legend = FALSE,
                                            show_column_names = TRUE)

######## plotting all heat maps together ----------------

map_list<-(pcaFactors+
  amstarLevelsPlot +
  topicAreaPlot +
  articlePageCountPlot +
  numAuthorsPlot +
  numInstitutionsPlot +
  numAuthorsConflictInterestPlot +
  fundingPharmaPlot +
  fundingAcademicPlot +
  clinicalTrialsPlot +
  observationalStudiesPlot +
  srPlot +
  metaanalysesIncludedPlot +
  economicStudiesPlot +
  journalImpactFactorPlot +
  articleInfluenceScorePlot +
  fiveYearImpactFactorPlot +
  citesInGoogleScholarPlot +
  citesInWoSPlot)

postscript("allHeatMaps.eps", 
           width = 2000, 
           height = 1000)
draw(map_list)
graphics.off()

tiff("allHeatMaps.tiff", 
     width = 2000, 
     height = 1000, 
     pointsize = 1/300, 
     units = 'in', 
     res = 300)
draw(map_list)
dev.off()

######## get the items by cluster ----------------

itemsByCluster<-cutree(dend, k = 6)
write.table(itemsByCluster, file="itemsByCluster.csv", quote=F, sep="\t", col.names=NA, append=T)
