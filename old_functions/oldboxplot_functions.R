dataFormatter <- function(data, anno=FALSE, labels){
  labels <- read.delim(labels$datapath, sep = "\t", header = T)
  data <- data.frame(read.delim(data$datapath, sep = "\t", header = T))
  
  data <- data %>%  pivot_longer(!Gene, names_to="SampleID", values_to="Value")
  data <- data %>% arrange(SampleID,Gene)
  data <- merge(x = data, y = labels, by.x = "SampleID", by.y = "Sample", all.x = TRUE)
  print(data)
  if (!is.null(anno)){
    annotations <- data.frame(read.delim(anno$datapath, sep = "\t", header = T))
    annotations <- annotations[, c("Gene", "gene_name")]
    data <- merge(x = data, y = annotations, by="Gene", all.x=TRUE)
  }else{
    names(data)[names(data) == 'Gene'] <- 'gene_name'
  }
  return(data)
}


boxplotPlotter <- function(data, genenames, combine, transformation, dots = FALSE) {
  data <- data %>% filter(gene_name %in% genenames)
  data <- unite(data, "Grouped", combine, remove = FALSE)
  if (transformation != 0) {
    data$Value <- data$Value + transformation
    data$Value <- log2(data$Value)
  }
  p <- ggplot(data, aes(x = Grouped, y = Value)) +
    geom_boxplot() +
    facet_grid(cols = vars(gene_name))+ 
    ylab("Value") +
    theme(legend.title = element_blank(),
          legend.position = "none", 		  
		  axis.text.x = element_text(angle = 90)		  
		  )
  if (transformation != 0) {
    p <- p + ylab(paste("Log2(Value) + ", transformation))
    data$Value <- log2(data$Value)
  }
  if (dots == TRUE) {return(ggplotly(p + geom_point(size = 1, alpha = 0.8, position=position_jitter(width=0.1, height=0.1))))}
  else {return(ggplotly(p))}
}

