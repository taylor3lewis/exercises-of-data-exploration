install.packages("ggplot2")
library(ggplot2)
options("scipen"=999, "digits"=4)
c_db = read.table("POP.txt", header=T, sep="\t")
clean_db <- na.omit(c_db)

srt_data_pop <- data.frame(name=character(0), pop=integer(0))
for(name in unique(clean_db$CountryName)){
  if(nchar(name)<50){
    len_measures <- length(subset(clean_db, CountryName==name)$Birth_rate)
    total_pop_mean <- sum(as.numeric(subset(clean_db, CountryName==name)$Birth_rate)) / len_measures
    if(total_pop_mean!=0){
      temp <- data.frame(name=name, pop=total_pop_mean)
      srt_data_pop <- rbind(srt_data_pop, temp)
    }
  }
}
order_data <- srt_data_pop[with(srt_data_pop, order(pop, decreasing=TRUE)), ]
top20 <- tail(order_data, n = 20)

# for(name1 in top20$name){}

ggplot(top20, aes(x=reorder(name, -pop), y=pop, fill=reorder(name, -pop), label=name)) +   
  geom_bar(position='dodge', stat='identity', color='black') + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Países com as 20 menores taxas médias de natalidade, entre 1990 a 2014") + 
  geom_label(position="stack", size=3)