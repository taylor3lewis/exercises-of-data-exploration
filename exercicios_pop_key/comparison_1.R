# install.packages("ggplot2")
library(ggplot2)
library(ggrepel)
options("scipen"=999, "digits"=4)
c_db = read.table("POP.txt", header=T, sep="\t")
clean_db <- na.omit(c_db)

# Mostre , compare e analise a evolução histórica das taxas de mortalidade, 
# entre 1990 e 2014, para o Brasil, Estados Unidos e China.

br_data <- subset(subset(clean_db, CountryName=="Brazil"), Death_rate>0)
ch_data <- subset(subset(clean_db, CountryName=="China"), Death_rate>0)
eu_data <- subset(subset(clean_db, CountryName=="United States"), Death_rate>0)

all_data <- NULL
all_data <- rbind(br_data, ch_data)
all_data <- rbind(all_data, eu_data)

str(all_data)

ggplot(all_data, 
       aes(x=Time, y=Death_rate, fill=CountryName, label=CountryName, colour=CountryName)
       ) +   
  geom_line(aes(x=Time, y=Death_rate)) + 
  geom_point() +
  scale_colour_manual(values=c("#00AA00","#AA0000","#0000AA")) +
  labs(y="Taxa de Mortalidade (por 1.000)")

