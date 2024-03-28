# #data
# test<-read.table("data_scatterplot_example.txt",header=T,row.names = 1,sep="\t")
# #default
# scatter_plot(xval=test[,1],yval=test[,2])
# #change parameters
# scatter_plot(xval=test[,1],yval=test[,2],colorby = test[,3],sizeby= test[,1],
#shapeby=test[,3],xlab="FE207702_Day18 vs Ctrl_Day18",ylab="FE207702_Day19 vs Ctrl_Day19",colors=c("orange","black","red","blue"),xlim=c(-6,6),ylim=c(-6,6),regress=T,cor=T)
#

scatter_plot <- function(xval, yval, colorby = NULL, shapeby = NULL, sizeby = NULL,
                         xlim = NULL, ylim = NULL, xlab = "", ylab = "",
                         na.val = 0, na.rm = T, main = "", colors = NULL, regress = T, cor = T) {
  # remove/replace NA values
  if (na.rm) {
    val.sel <- !is.na(xval) & !is.na(yval)
    xval <- xval[val.sel]
    yval <- yval[val.sel]
    if (!is.null(colorby)) {
      colorby <- colorby[val.sel]
    }
    if (!is.null(shapeby)) {
      shapeby <- shapeby[val.sel]
    }
    if (!is.null(sizeby)) {
      sizeby <- as.numeric(sizeby[val.sel])
    }
  } else {
    # xval[!is.na(xval)] <- na.val
    # yval[!is.na(xval)] <- na.val
  }
  # convert x/y to numeric value
  # xval <- as.numeric(xval)
  # yval <- as.numeric(yval)
  # create df for plot
  data <- data.frame(xval, yval)
  # add color,shape, size inforamtion
  if (!is.null(colorby)) {
    data$colorby <- colorby
  }
  if (!is.null(shapeby)) {
    data$shapeby <- shapeby
  }
  if (!is.null(sizeby)) {
    data$sizeby <- abs(as.numeric(sizeby))
  }
  # plot
  plot <- ggplot(data, aes(x = xval, y = yval, color = colorby, shape = shapeby, size = sizeby))
  plot <- plot + geom_point()
  # choose color by names, need to be expanded by customized selection
  if (!is.null(colors)) {
    names(colors) <- levels(as.factor(colorby))
    plot <- plot + scale_colour_manual(name = "colorby", values = colors)
  }
  # cusomized shape, need to be expanded by customized selection
  # to be implemented
  if (!is.null(shapeby)) {
    plot <- plot + scale_shape_discrete(name = "shapeby")
  }
  # customized size, need to be expanded by customized selection
  if (!is.null(sizeby)) {
    plot <- plot + scale_size_continuous(name = "sizeby")
  }
  # xlim & ylim
  if (!is.null(xlim)) {
    plot <- plot + xlim(xlim)
  } else {
    xlim <- range(xval)
  }
  if (!is.null(ylim)) {
    plot <- plot + ylim(ylim)
  } else {
    ylim <- range(yval)
  }
  # title, background, xlab, ylab
  plot <- plot + ggtitle(label = main) +
    xlab(xlab) + # Change X-Axis label
    ylab(ylab) + # Change Y-Axis label
    theme_bw(base_size = 14) + # change overall theme
    # theme(legend.position = "right",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    # plot.title = element_text(size = 10, face = "bold",hjust = 0.5),axis.title=element_text(size=8)) # change the legend
    theme(
      legend.position = "right", panel.grid.minor = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5), axis.title = element_text(size = 8)
    ) # change the legend
  
  # regression line
  if (regress) {
    # below line doesn't work well with multiple color/shape setting
    # plot <-plot +geom_smooth(data=data,method = "lm", se = F,aes(x = xval, y =yval),color="black")
    data.lm <- lm(yval ~ xval, data)
    plot <- plot + geom_abline(slope = coef(data.lm)[[2]], intercept = coef(data.lm)[[1]])
  }
  
  # correlation
  if (cor) {
    pearsoncor <- round(cor(xval, yval, method = "pearson", use="pairwise.complete.obs"), 3)
    spearmancor <- round(cor(xval, yval, method = "spearman", use="pairwise.complete.obs"), 3)
    print(pearsoncor)
    print(spearmancor)
    plot <- plot + annotate("text", x = (xlim[2] - xlim[1]) * 0.1 + xlim[1], y = -(ylim[2] - ylim[1]) * 0.1 + ylim[2], label = paste("Pearson Cor:", pearsoncor, "\n", "Spearman Cor:", spearmancor, sep = ""), colour = "black")
  }
  return(ggplotly(plot))
}
