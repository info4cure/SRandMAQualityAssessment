#########
# Juan Ruano


##### world map plot AMSTAR scores ----------------
##  R packages --------

library(RColorBrewer)
library(ggplot2)
library(maptools)
library(rgdal)
library(maps)
library(mapproj)
library(data.table)

##  file environment setting --------
# For RStudio only 

setwd(dirname(rstudioapi::callFun("getActiveDocumentContext")$path)) 

##  file read --------

file2                               <- utils::read.csv2("dbWorldMapAMSTAR.csv", 
                                                 sep=";", 
                                                 dec=".", 
                                                 header=TRUE)
DB_worldMap                         <- base::as.data.frame(file2)
DB_worldMap_mean                    <- base::aggregate(AMSTAR_consensus_2 ~ region, data = DB_worldMap, FUN = mean)
DB_worldMap_mean$AMSTAR_consensus_2 <- base::round(DB_worldMap_mean$AMSTAR_consensus_2,0)


### no ejecutar normalmente
map.world                           <- ggplot2::map_data(map = "world")
map.world                           <- map.world[map.world$region != "Antarctica",] 
map.world                           <- data.table::data.table(map.world)
setkey(map.world,region)
DB_worldMap_mean                    <- data.table::data.table(DB_worldMap_mean)
setkey(DB_worldMap_mean,region)
map.df_2                            <- base::merge(map.world,DB_worldMap_mean, all=TRUE)
map.df_2                            <- map.df_2[order(map.df_2$region),]

## plot --------
ggplot2::ggplot(map.df_2, aes(x = long, y = lat, group = group, fill = AMSTAR_consensus_2), alpha=1/4) +
ggplot2::geom_polygon(colour = "darkgrey", size = 0.1) +
ggplot2::coord_equal() +
ggplot2::scale_fill_distiller(palette='Spectral',  trans = "reverse", breaks = c(1,2,3,4,5,6,7,8,9,10,11)) +
ggplot2::guides(fill = guide_legend(reverse = TRUE, guide_legend(title = "mean AMSTAR"))) +
ggplot2::theme(legend.position="right") +
ggplot2::scale_x_continuous(expand = c(0,0)) +
ggplot2::scale_y_continuous(expand = c(0,0)) +
ggplot2::theme(axis.line = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
       axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
ggplot2::labs(title = "")


