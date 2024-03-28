volcano_helper <- function(data, gene, fc, q, sig, xlab, ylab, q_cutoff, fc_cutoff) {
  gene <- data[[gene]]
  fc <- data[[fc]]
  q <- data[[q]]
  sig <- data[[sig]]
  q_cutoff <- as.numeric(q_cutoff)
  fc_cutoff <- as.numeric(fc_cutoff)

  return(enhanced_volcano_plot(gene = gene, fc = fc, q = q, sig = sig, xlab = xlab, ylab = ylab, main = "main",
                               fc_cutoff = fc_cutoff, q_cutoff = q_cutoff)
  )
}

# volcano_plot_ggplot <- function(fc, q, sig, xlim = c(-7, 7), ylim = c(0, 30),
#                                 xlab = "Log2FC", ylab = "-log10 P", main = "Volcano Plot",
#                                 fc_cutoff, q_cutoff) {

enhanced_volcano_plot <- function(gene, fc, q, sig, labels = NULL,
                                  upcol = "red2", downcol = "blue2",
                                  xlab = "Log2FC", ylab = "-log10 P", main = "Volcano Plot",
                                  fc_cutoff = 0, q_cutoff = 0, xlim = c(-10, 10), ylim = c(0, 30)) {
  # Create a data frame using gene names, fold changes, and q-values
  fc <- as.numeric(unlist(fc))
  q <- as.numeric(unlist(q))
  q[is.na(q)] <- 1
  sig[is.na(sig)] <- 0
  gene <- make.names(as.character(unlist(gene)), unique = T) # changed here to allow duplicated gene names
  # remember the numbers
  up.number <- sum(sig == 1)
  down.number <- sum(sig == -1)
  total.number <- length(sig)
  # decide xlim/ylim
  # Determine balanced x and y-axis limits (no features omitted)
  # y-axis upper limit
  if (min(q) > 0) {
    max_pval <- -log10(min(q)) + 0.1
  } else {
    max_pval <- -log10(min(q[q > 0]) / 10) + 0.1 # changed here for pval=0
  }
  if (length(ylim) > 1) {
    if (max_pval < max(ylim)) {
      ylim <- c(0, max_pval)
    }
  } else {
    ylim <- c(0, max_pval)
  }
  # To get a symmetrical x-axis
  max_fc <- max(fc, na.rm = T)
  min_fc <- min(fc, na.rm = T)
  if (max_fc > abs(min_fc)) {
    min_fc <- -max_fc
  } else {
    max_fc <- -min_fc
  }
  if (length(xlim) > 1) {
    if (max_fc < max(xlim)) {
      xlim <- c(min_fc, max_fc)
    }
  } else {
    xlim <- c(min_fc, max_fc)
  }
  # filtered data
  newfc <- fc[abs(fc) <= max(xlim) & q >= 10^-max(ylim)]
  newq <- q[abs(fc) <= max(xlim) & q >= 10^-max(ylim)]
  newsig <- sig[abs(fc) <= max(xlim) & q >= 10^-max(ylim)]
  newgene <- gene[abs(fc) <= max(xlim) & q >= 10^-max(ylim)]
  # data used by volcano
  df <- data.frame(gene_name = as.character(newgene), lfc = newfc, q = newq, stringsAsFactors = F)
  rownames(df) <- newgene
  # Create a named vector of custom colors
  col_scheme <- c("Up" = upcol, "Down" = downcol, "N.S." = "grey")
  cols <- rep(col_scheme["N.S."], length(newsig))
  cols[newsig == 1] <- col_scheme["Up"]
  cols[newsig == -1] <- col_scheme["Down"]
  names(cols)[cols == col_scheme["Up"]] <- "Up"
  names(cols)[cols == col_scheme["Down"]] <- "Down"
  names(cols)[cols == col_scheme["N.S."]] <- "N.S."
  # Label top 10 Up and down genes (based on FC) if user does not
  # provide a vector of genes to label on volcano plot
  if (is.null(labels) == TRUE) {
    tmp.df <- data.frame(newgene, newfc, cols)
    rownames(tmp.df) <- rownames(df)
    tmp.df <- tmp.df[tmp.df$cols != col_scheme["N.S."], ]
    tmp.df <- tmp.df[order(tmp.df$newfc), ]
    labels <- c(rownames(tmp.df)[1:10], tail(rownames(tmp.df), n = 10))
  }
  # Render the volcano plot
  plt <- EnhancedVolcano(df,
    x = "lfc", y = "q", lab = df$gene_name,
    pCutoff = q_cutoff, FCcutoff = fc_cutoff,
    gridlines.major = FALSE, gridlines.minor = FALSE,
    drawConnectors = F, legendLabSize = 12,
    cutoffLineCol = "red", colAlpha = 0.75,
    cutoffLineType = "dashed", border = "full",
    colCustom = cols, legendPosition = "right",
    pointSize = 2, cutoffLineWidth = 0.4,
    labFace = "plain", labSize = 4, subtitle = main,
    # ylim = c(0, max_pval), xlim = c(min_fc, max_fc),
    ylim = ylim, xlim = xlim,
    axisLabSize = 12, captionLabSize = 12,
    xlab = xlab, ylab = ylab, title = "",
    caption = paste0("Total = ", total.number, " features"),
    typeConnectors = "closed", legendIconSize = 2,
    selectLab = labels, borderWidth = 1.5
  )
  # changed drawConnectors = T,labSize=3
  # Add numbers of up and down DE genes to the plot
  plt <- plt + geom_text(
    x = min(xlim), y = max(ylim), label = down.number,
    col = col_scheme["Down"], size = 5
  )
  plt <- plt + geom_text(
    x = max(xlim), y = max(ylim), label = up.number,
    col = col_scheme["Up"], size = 5
  )
  return(plt) # generate plot
}
