#Function to find group names from label file
pca_find_groups <- function(labels, group) {
  #labels <- read.delim(labels, sep = "\t", header = T)
  groups <- unique(labels[[group]])
  return(groups)
}

#Function to run PCA analysis, returns PCA dataframe and variance list
pca_analysis <- function(data, labels, transformation) {
  #Import file, replace N/A with 0
  #data <- read.delim(data, sep = "\t", header = T, row.names = 1, check.names = F)
  data[is.na(data)] <- 0
  #if transformation is wanted, do transformation
  if (transformation != 0) {
    data <- data + transformation
    data <- log2(data)
  }
  print("-1")
  #perform PCA
  data <- t(data)
  data <- data[, apply(data, 2, var, na.rm = T) != 0]
  print("-0.7")
  #data <- t(tidyr::drop_na(as.data.frame(t(data))))
  print(t(data))
  print(dim(data))
  data <- prcomp(data, center = T, scale. = T)
  
  print("-0.5")
  variance <- summary(data)$importance[2, ]
  data <- data$x
  print("0")
  return(list("data" = data, "variance" = variance))
}

#Function to generate plotly plot with user customization
pca_plot <- function(data, labels, colorgroup,shapegroup, x, y, showEllipse, color, shape, custom, showLabels, showGroupLabels, variance) {
  print("1")
  #Get group names from label file, attatch to dataframe
  colorgroups <- pca_find_groups(labels, colorgroup)
  shapegroups <- pca_find_groups(labels, shapegroup)
  
  print("2")
  #labels <- read.delim(labels, sep = "\t", header = T)
  
  #df for plot
  data <- data.frame(data)
  
  colorlabels <- labels[[colorgroup]]
  data$colorgroups <- colorlabels

  shapelabels <- labels[[shapegroup]]
  data$shapegroups <- shapelabels
  
    
  print("3")
  #get axis label names from dataframe and variance list
  xlab = paste(colnames(data)[x], " ", 100*variance[colnames(data)[x]], "%", sep = "")
  ylab = paste(colnames(data)[y], " ", 100*variance[colnames(data)[y]], "%", sep = "")
  #create ggplot plot object, aes(text) is the hover tooltip for each point
  plot <- ggplot(data = data, show.legend = FALSE, 
                 aes(x = data[, x], y = data[, y], color = colorgroups, shape = shapegroups, 
                     text = paste("</br>", rownames(data), "</br>", colnames(data)[x], ": ",  data[, x], "</br>", colnames(data)[y], ": ", data[, y])
                 )
  )
  
  #attatch axis labels on plot
  plot <- plot + labs(x = xlab, y = ylab)
  
  #if Custom checkbox is not selected
  if (!custom) {
    if (shape == "ByGroup") {
      plot <- plot + geom_point(show.legend = TRUE)
      plot <- plot + scale_shape_discrete(name="shapeby") #set point according to group setting
    } else if (shape == "Circle") { #if selected shape is a circle
      #shape <- as.list(rep(16, length(colorgroups)))
      shape <- as.list(rep(16, nrow(data)))
      plot <- plot + geom_point(show.legend = TRUE)
      plot <- plot + scale_shape_manual(values = shape) #set point shapes to circle
    } else if (shape == "Square") { #square
      shape <- as.list(rep(15, length(colorgroups)))
      plot <- plot + geom_point(show.legend = TRUE)
      plot <- plot + scale_shape_manual(values = shape)
    } else if (shape == "Label Only") { #if label only is selected, plot points using label names instead of a point
      plot <- plot + geom_text(label = rownames(data), show.legend = TRUE)
    }
    
    if (showLabels) { #if show label option is on, if "Label Only" option is not selected, show labels for each point
      if (shape != "Label Only") {
        plot <- plot + geom_text(label = rownames(data), nudge_y = 5)
      }
    }
    
    #color options
    #option 1 is the default ggplot color palette
    #Spectral and Dark2 are RColorBrewer palettes
    if (color == "Palette 2") {
      plot <- plot + scale_color_brewer(palette = "Spectral")
    } else if (color == "Palette 3") {
      plot <- plot + scale_color_brewer(palette = "Dark2")
    }
  }
  
  #if custom option is selected
  if (custom) {
    plot <- plot + geom_point(show.legend = TRUE)
    plot <- plot + scale_color_manual(values = color)
    plot <- plot + scale_shape_manual(values = shape)
    if (showLabels) {
      plot <- plot + geom_text(label = rownames(data), nudge_y = 5)
    }
  }
  
  #if ellipse option is selected, use stat_ellipse to find ellipse for each group
  if (showEllipse) {
    plot <- plot + stat_ellipse(show.legend = FALSE, inherit.aes = FALSE, 
                                aes(color = colorgroups, x = data[, x], y = data[, y]))
  }
  
  #if show group label option is selected, use stat_mean function (from ggpubr) to calculate group means
  #stat_mean function is originally from ggpubr, but the function can be found at bottom of this script
  if (showGroupLabels) {
    plot <-plot + stat_mean(geom = "text", size = 4 , show.legend = FALSE, inherit.aes = FALSE, 
                            aes( color = colorgroups, label = colorgroups, x = data[, x], y = data[, y])
    )
  }
  
  #Add frame to plot, remove background
  plot <- plot + theme(
    panel.border = element_rect(fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_blank()
  )
  
  #Convert ggplotly object to plotly
  #tooltip=c("text") uses the tooltip format previously defined
  plot <- ggplotly(plot, tooltip = c("text"))
  #plot <- plot %>% layout(autosize = F, width = 5000, height = 5000)
  return(plot)
}



#Group mean function taken from ggpubr
stat_mean <- function(mapping = NULL, data = NULL, geom = "point", position = "identity", na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE, ...) {
  layer(stat = StatMean, data = data, mapping = mapping, geom = geom, position = position, 
        show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}
StatMean <- ggproto(
  "StatMean",
  Stat,
  required_aes = c("x", "y"),
  compute_group = function(data, scales) {
    res <- data.frame(x = mean(data$x, na.rm = TRUE), y = mean(data$y, na.rm = TRUE))
  }
)