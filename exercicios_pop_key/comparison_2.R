# install.packages("ggplot2")
library(ggplot2)
library(ggrepel)
options("scipen"=999, "digits"=4)
c_db = read.table("POP.txt", header=T, sep="\t")
clean_db <- na.omit(c_db)

# unique(clean_db$CountryName)

# Mostre , compare e analise a evolução histórica do % de população >65 anos, 
# entre 1990 e 2014, para a França, Argentina e África do Sul.

fr_data <- subset(subset(clean_db, CountryName=="France"), Population_ages_over65>0)
ar_data <- subset(subset(clean_db, CountryName=="Argentina"), Population_ages_over65>0)
sa_data <- subset(subset(clean_db, CountryName=="South Africa"), Population_ages_over65>0)

all_data <- NULL
all_data <- rbind(fr_data, ar_data)
all_data <- rbind(all_data, sa_data)

str(all_data)

ggplot(all_data, 
       aes(x=Time, y=Population_ages_over65, fill=CountryName, label=CountryName, colour=CountryName)
) +   
  geom_line(aes(x=Time, y=Population_ages_over65)) + 
  geom_point() +
  scale_colour_manual(values=c("#00AA00","#AA0000","#0000AA")) +
  labs(y="Comparação: % de pessoas acima dos 65 anos.")

