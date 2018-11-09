install.packages("ggplot2")
install.packages("ggrepel")
library(ggplot2)
library(ggrepel)
options("scipen"=999, "digits"=4)
c_db = read.table("POP.txt", header=T, sep="\t")
clean_db <- na.omit(c_db)
data2014 <- subset(clean_db, Time==2014)

data2014

srt_data_pop <- data.frame(name=character(0), pop=integer(0))
for(name in unique(data2014$CountryName)){
  if(nchar(name)<50){
    pop_total <- subset(data2014, CountryName==name)$Population_total
    if(pop_total!=0){
      print(name)
      temp <- data.frame(name=name, pop=pop_total)
      srt_data_pop <- rbind(srt_data_pop, temp)
    }
  }
}

order_data <- srt_data_pop[with(srt_data_pop, order(pop, decreasing=TRUE)), ]

order_data

top50 <- head(order_data, n = 50)

top50

ggplot(top50, aes(x=reorder(name, -pop), y=pop, fill=reorder(name, -pop), label=name)) +   
  geom_bar(position='dodge', stat='identity', color='black') + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom") +
  labs(y="Os 50 Países com maior população, considerando dados de 2014") + 
  geom_label(position="stack", size=3)
