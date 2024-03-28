fig <- plot_ly(z = ~volcano) %>% add_surface()
orca(fig, "surface-plot.png")