volcano_helper <- function(data, fc, q, sig, xlab, ylab, q_cutoff, fc_cutoff){
  fc = data[[fc]]
  q = data[[q]]
  sig = data[[sig]]
  q_cutoff = as.numeric(q_cutoff)
  fc_cutoff = as.numeric(fc_cutoff)
  
  return(volcano_plot_ggplot(fc=fc, q=q, sig=sig, xlab=xlab,
                                      ylab=ylab, main=" ", q_cutoff=q_cutoff, fc_cutoff=fc_cutoff))
}

volcano_plot_ggplot <- function(fc, q, sig, xlim = c(-7, 7), ylim = c(0, 30), 
                                xlab = "Log2FC", ylab = "-log10 P", main = "Volcano Plot", 
                                fc_cutoff, q_cutoff) {
  q[is.na(q)] <- 1 # remove NA
  
  fc <- as.numeric(unlist(fc))
  q <- as.numeric(unlist(q))
  # define color
  cols <- c("Up" = "red", "Down" = "green", "N.S." = "grey")
  shs <- c("21" = 21, "24" = 24)
  # define col and shape
  shapes <- rep(21, length(fc))
  shapes[abs(fc) > xlim[2]] <- 24
  shapes[-log10(q) > ylim[2]] <- 24
  sig.new <- rep("N.S.", length(sig))
  sig.new[sig == 1] <- "Up"
  sig.new[sig == -1] <- "Down"
  # redefine cols and shs
  cols.sel <- cols[levels(as.factor(sig.new))]
  shs.sel <- shs[levels(as.factor(shapes))]
  # transform data
  fc[fc > xlim[2]] <- xlim[2]
  fc[fc < xlim[1]] <- xlim[1]
  q[-log10(q) > ylim[2]] <- 10^-ylim[2]
  # defined cols and shapes
  data <- data.frame(lfc = fc, q = -log10(q), sig = sig.new, shape = shapes)
  # plot
  vol <- ggplot(data, aes(x = lfc, y = q, fill = sig, shape = factor(shape)))
  vol <- vol + ggtitle(label = main) +
    geom_point(size = 2, alpha = 1, na.rm = T, colour = "black") +
    scale_fill_manual(name = "Color", values = cols) +
    scale_shape_manual(name = "Shape", values = shs) +
    theme_bw(base_size = 14) + # change overall theme
    theme(
      legend.position = "right", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5), axis.title = element_text(size = 8)
    ) + # change the legend
    guides(fill = guide_legend(override.aes = list(size = 3, colour = cols.sel), title = "Significance"), shape = "none") +
    xlab(xlab) + # Change X-Axis label
    ylab(ylab) + # Change Y-Axis label
    ylim(ylim) +
    scale_x_continuous(breaks = seq(xlim[1], xlim[2], 1), lim = xlim) +
    geom_hline(yintercept = -log10(as.numeric(q_cutoff)), colour = "#990000", linetype = "dashed") + # p cutoff
    geom_vline(xintercept = as.numeric(fc_cutoff), colour = "#990000", linetype = "dashed") + geom_vline(xintercept = -as.numeric(fc_cutoff), colour = "#990000", linetype = "dashed") + # fc cutoff line
    annotate("text", x = xlim[2] - 0.5, y = ylim[2] - 0.5, label = length(which(sig == 1)), colour = "red", size = 5) +
    annotate("text", x = xlim[1] + 0.5, y = ylim[2] - 0.5, label = length(which(sig == -1)), colour = "green", size = 5) +
    annotate("text", x = xlim[2] - 1, y = -log10(as.numeric(q_cutoff)) + 0.5, label = substring(main, 28), colour = "red", size = 3)
  return(ggplotly(vol))
}
