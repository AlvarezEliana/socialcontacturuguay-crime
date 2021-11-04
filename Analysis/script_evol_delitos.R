##  Make the evo_delitos.pdf figure

## setwd("C:\\Users\\Usuario\\Desktop\\Droga_Delitos_Repetto")
## install.packages("gridextra")

base <- rio::import("evo_delitos.xlsx")

head(base)

library(ggplot2)
library(gridExtra)
library(here)

# Grafico rapiñas
graph1 <- ggplot(base, aes(x = Year, y = Assaults)) +
  geom_line(color = "red")

# Grafico homicidios
graph2 <- ggplot(base, aes(x = Year, y = Homicides)) +
  geom_line(color = "blue")

pdf(file = here::here("media", "evo_delitos.pdf"))
print(grid.arrange(graph1, graph2, nrow = 1))
dev.off()
