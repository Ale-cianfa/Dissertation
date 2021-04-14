## VISUALIZATION FOR ENVIRONMENTAL PARAMETERS

## Visualizing raster data----

### Bathymetry:

bathymetry <- raster("Parameters/bathymetry/bat_1.tif")

bat_TID <- raster("Parameters/bathymetry/bat_2_TID.tif")

bathymetry #to get the properties

plot(bathymetry)

#png("img/bat.png", width = 6, height = 4, units = "in", res = 300)

#image(bathymetry, col = viridis_pal(option = "D", direction = -1)(10), main = "Bathymetry of Iceland")

#dev.off()

#OPTION = A character string indicating the colormap option to use. 
#Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), 
#"viridis" (or "D", the default option) and "cividis" (or "E").

## plotting bathymetry with ggplot 

display.brewer.all()

bat_df <- as.data.frame(bathymetry, xy = TRUE, na.rm = TRUE)

bat_df <- bat_df %>% filter(bat_1 < 0)

(bat_plot <- ggplot() +
    geom_raster(data = bat_df, aes(x = x, y = y, fill = bat_1)) +
    #scale_fill_viridis_c() +
    scale_color_brewer(palette = "BuPu") +
    coord_quickmap() +
    ggtitle("North of Iceland Bathymetric profile") +
    ylim(64.5,68) +
    xlim(-27, -10) +
    theme_classic() +
    theme(legend.position = "right",
          legend.title = element_text(size = 13, face ="bold"),
          legend.text = element_text(size = 12)) + # removes defalut grey background
    theme(plot.title = element_text(size = 15, face ="bold", hjust = 0.5),             # centres plot title
          text = element_text(size=20),		       	    # font size
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(fill = "Depth (m)", 
         x = "longitude", 
         y ="latitude")) # rotates x axis text

ggsave(bat_plot, file = "img/bat_plot3.png", height = 5, width = 9)

### Chlorophyll:

chlor_0103 <- raster("Parameters/chlor_a/chlor_0103.tif")
chlor_0104 <- raster("Parameters/chlor_a/chlor_0104.tif")
chlor_0105 <- raster("Parameters/chlor_a/chlor_0105.tif")
chlor_0106 <- raster("Parameters/chlor_a/chlor_0106.tif")
chlor_0107 <- raster("Parameters/chlor_a/chlor_0107.tif")
chlor_0108 <- raster("Parameters/chlor_a/chlor_0108.tif")

## Creating a data frame for each year to show the progress in a facet plot

chlor_0103_df <- as.data.frame(chlor_0103, xy = TRUE, na.rm = TRUE)
chlor_0104_df <- as.data.frame(chlor_0104, xy = TRUE, na.rm = TRUE)
chlor_0105_df <- as.data.frame(chlor_0105, xy = TRUE, na.rm = TRUE)
chlor_0106_df <- as.data.frame(chlor_0106, xy = TRUE, na.rm = TRUE)
chlor_0107_df <- as.data.frame(chlor_0107, xy = TRUE, na.rm = TRUE)
chlor_0108_df <- as.data.frame(chlor_0108, xy = TRUE, na.rm = TRUE)


## Making the facet plot 

(chlor_facet_01_07 <- ggplot() +
    geom_raster(data = chlor_0107_df, aes(x = x, y = y, fill = chlor_0107)) +
    scale_fill_viridis_c() +
    coord_quickmap() +
    ggtitle("Chlorophyll") +
    xlab("Longitude") +
    ylab("Latitude") +
    ylim(62,68) +
    xlim(-27, -10) +
    theme_classic() +   					    # removes defalut grey background
    theme(plot.title = element_text(hjust = 0.5),             # centres plot title
          text = element_text(size=20),		       	    # font size
          axis.text.x = element_text(angle = 90, hjust = 1)))  # rotates x axis text



























