setwd("C:\\Users\\Usuario\\Desktop\\Droga_Delitos_Repetto")
install.packages("gridextra")

base <- rio::import("Evolucion historica rapiñas y homicidios_ COMPLETO.xlsx")

head(base)

library(ggplot2)
library(gridExtra)

#Grafico rapiñas
graph1 <- ggplot(base, aes(x=Year, y=Assaults)) +
  geom_line(color="red") 


#Grafico homicidios
graph2 <- ggplot(base, aes(x=Year, y=Homicides)) +
  geom_line(color="blue")

grid.arrange(graph1, graph2, nrow = 1)
