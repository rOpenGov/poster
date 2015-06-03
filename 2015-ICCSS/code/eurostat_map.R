library(eurostat)
library(tidyr)

df <- get_eurostat("tgs00026", time_format = "raw") 
df$time <- eurotime2num(df$time)
df <- df[df$time == max(df$time),]
download.file("http://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/NUTS_2010_60M_SH.zip", destfile="NUTS_2010_60M_SH.zip")
unzip("NUTS_2010_60M_SH.zip")
library(rgdal)
map <- readOGR(dsn = "./NUTS_2010_60M_SH/Data", layer = "NUTS_RG_60M_2010")
map_nuts2 <- subset(map, STAT_LEVL_ == 2)
NUTS_ID <- as.character(map_nuts2$NUTS_ID)
VarX <- rep(NA, 316)
dat <- data.frame(NUTS_ID,VarX)
dat2 <- merge(dat,df,by.x="NUTS_ID",by.y="geo", all.x=TRUE)
row.names(dat2) <- dat2$NUTS_ID
row.names(map_nuts2) <- as.character(map_nuts2$NUTS_ID)
## order data
dat2 <- dat2[order(row.names(dat2)), ]
map_nuts2 <- map_nuts2[order(row.names(map_nuts2)), ]
## join
library(maptools)
dat2$NUTS_ID <- NULL
shape <- spCbind(map_nuts2, dat2)
library(ggplot2)
library(rgeos)
shape$id <- rownames(shape@data)
map.points <- fortify(shape, region = "id")
map.df <- merge(map.points, shape, by = "id")
map.df$unit <- as.character(map.df$unit)
library(ggplot2)
library(scales)
library(grid)
p <- ggplot(data=map.df, aes(long,lat,group=group))
#p <- p + geom_polygon(data = map.df, aes(long,lat),fill=NA,colour="white",size = .2)
p <- p + geom_polygon(aes(fill = values),colour="white",size=.2)
p <- p + coord_map(project="orthographic", xlim=c(-12,34), ylim=c(35,70))
p <- p + labs(title = paste0("Disposable household incomes in ",max(df$time)))
p <- p +  theme(legend.position = c(0.23,0.70), 
                legend.justification=c(0,0),
                legend.key.size=unit(20,'mm'),
                legend.direction = "vertical",
                legend.background=element_rect(colour=NA, fill=alpha("white", 2/3)),
                legend.text=element_text(size=30), 
                legend.title=element_text(size=30), 
                title=element_text(size=30), 
                panel.background = element_blank(), 
                plot.background = element_blank(),
                panel.grid.minor = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                panel.grid.major = element_line(colour = 'Grey80', size = .5, linetype = 'solid'),
                axis.text = element_blank(), 
                axis.title = element_blank(), 
                axis.ticks = element_blank(), 
                plot.margin = unit(c(-3,-1.5, -3, -1.5), "cm"))
p <- p + guides(fill = guide_legend(title = "Euro per Year",
                                    title.position = "top", 
                                    title.hjust=0))