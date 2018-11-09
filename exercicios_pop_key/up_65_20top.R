# install.packages("ggplot2")
library(ggplot2)
options("scipen"=999, "digits"=4)
c_db = read.table("POP.txt", header=T, sep="\t")
clean_db <- na.omit(c_db)

srt_data_pop <- data.frame(name=character(0), pop=integer(0))
for(name in unique(clean_db$CountryName)){
  if(nchar(name)<50){
    len_measures <- length(subset(clean_db, CountryName==name)$Population_ages_over65)
    pop_over65 <- sum(as.numeric(subset(clean_db, CountryName==name)$Population_ages_over65)) / len_measures
    if(pop_over65!=0){
      temp <- data.frame(name=name, pop=pop_over65)
      srt_data_pop <- rbind(srt_data_pop, temp)
    }
  }
}
order_data <- srt_data_pop[with(srt_data_pop, order(pop, decreasing=TRUE)), ]
top20 <- head(order_data, n = 20)

# for(name1 in top20$name){}

ggplot(top20, aes(x=reorder(name, -pop), y=pop, fill=reorder(name, -pop), label=name)) +   
  geom_bar(position='dodge', stat='identity', color='black') + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = "bottom") +
  labs(y="Países com as 20 maiores médias de população acima de 65 anos (entre 1990 e 2014)") + 
  geom_label(position="stack", size=3)
