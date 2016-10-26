#########
## Juan Ruano
## Polar plots of AMSTAR scores by journal (ranked by mean values)


##### world map plot AMSTAR scores ----------------
##  R packages --------
library(utils)
library(base)
library(reshape2)
library(stats)
library(ggplot2)

##  R functions --------
# Using the new ggproto mechanism available in ggplot2 2.0.0, coord_radar can be defined as

coord_radar <- function (theta = "x", start = 0, direction = 1) {
    theta <- match.arg(theta, c("x", "y"))
        r <- if (theta == "x") 
          "y"
        else 
          "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
        direction = sign(direction),
        is_linear = function(coord) FALSE)
  }


##  file environment setting --------
# For RStudio only 

setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path)) 


##  file read --------

file2               <- utils::read.csv2("dbPolarPlots.csv", sep=";", dec=".", header=TRUE)
DB_polar            <- base::as.data.frame(file2)
DB_polar_treatment  <- base::subset(DB_polar, topic=="treatment")


##  tyding dataset ONLY SR and MA for intervention studies --------

DB_polar_treatment$Journal_2 <- base::factor(DB_polar_treatment$Journal_2, levels = c("J Invest Dermatol","Clin Exp Dermatol", "Int J Dermatol", "JAMA Dermatol (Arch Dermatol)","J Am Acad Dermatol","Br J Dermatol","Dermatology","Arch Dermatol Res", "J Dermatol","J Cutan Med Surg","Acta Derm Venereol","Photodermatol Photoimmunol Photomed","J Eur Acad Dermatol Venereol","J Dtsch Dermatol Ges","J Dermatolog Treat","Pediatr Dermatol","Indian J Dermatol Venereol Leprol","Dermatol Online J","Dermatol Ther (Heidelb)","Am J Clin Dermatol"))
DB_polar_treatment_melted    <- reshape2::melt(DB_polar_treatment[c(2:13,15,16)],  id=c("AMSTAR_levels_2", "Journal_2", "num_publications"))
DB_polar_treatment_aggregate <- stats::aggregate(value ~ Journal_2 + variable+num_publications, data = DB_polar_treatment_melted, sum)


## polar plot --------

ggplot(DB_polar_treatment_aggregate, aes(x = variable, y = value/num_publications)) +
  geom_polygon(aes(group = Journal_2,color = Journal_2, size = num_publications),  fill = NA) +
  geom_line(aes(group = Journal_2, color = Journal_2, size = num_publications))+
  facet_wrap(~ Journal_2) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = rel(0.6)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())+
  xlab("") + ylab("") +
  guides(color = "none") +
  labs(size ="number of publications")+
  coord_radar()

