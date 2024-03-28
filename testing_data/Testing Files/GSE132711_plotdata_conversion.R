ad <- data.frame(read.delim("testing_data/GSE132711_PlotData_modifiedfdb.csv", sep = "\t", header = T))
ad <- as_tibble(ad)
print(ad)
ad <- ad %>%  pivot_wider(names_from = Symbol, values_from = l2fpkm)
ad <- as_tibble(cbind(nms = names(ad), t(ad)))
names(ad) <- ad[1,]
ad <- ad[-1,]
print(ad)
ad$THPR001A <-as.numeric(ad$THPR001A)
ad$THPR001B <-as.numeric(ad$THPR001B)
ad$THPR001C <-as.numeric(ad$THPR001C)
print(ad)


write.csv(ad, file = "testing_data/GSE132711_PlotData_final_wide.csv")

ad <- data.frame(read.delim("testing_data/GSE132711_PlotData_final_wide.csv", sep = ",", header = T))
print(ad)

ad <- ad[,-1]
print(ad)

write.table(ad, file='testing_data/GSE132711_PlotData_final_wide.csv', quote=FALSE, sep='\t', row.names = FALSE)
