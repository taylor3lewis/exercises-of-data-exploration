install.packages("ggplot2")
library(ggplot2)
options("scipen"=999, "digits"=4)
c_db = read.table("POP.txt", header=T, sep="\t")
clean_db <- na.omit(c_db)

clean_db$Population_total

srt_data_pop <- data.frame(name=character(0), pop=integer(0))
for(name in unique(clean_db$CountryName)){
  if(nchar(name)<50){
    if(total_pop_mean!=0){
      variation_pop <- var(subset(clean_db, CountryName==name)$Population_total)
      temp <- data.frame(name=name, pop=variation_pop)
      srt_data_pop <- rbind(srt_data_pop, temp)
    }
  }
}
order_data <- srt_data_pop[with(srt_data_pop, order(pop, decreasing=TRUE)), ]
top20 <- head(order_data, n = 20)

ggplot(top20, aes(x=reorder(name, -pop), y=pop, fill=reorder(name, -pop), label=name)) +   
  geom_bar(position='dodge', stat='identity', color='black') + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y="Lista de países (top 20) com maior variabilidade de população, entre 1990 e 2014") + 
  geom_label(position="stack", size=3)
