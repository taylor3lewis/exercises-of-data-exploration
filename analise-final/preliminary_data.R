library(ggplot2)
library(ggrepel)
library(gridExtra)
library(reshape2)
library(scales)
options("scipen"=999, "digits"=4)
data_db <- read.csv("exercicio.csv", header=T)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# Atributos existentes e sua tipificação;
# Variáveis Qualitativas (Nominais e ordinais) ;
# Variáveis Quantitativas (discretas e continuas);
# Calcule Media, Max, Min, Moda, Mediana, Variância e Desvio Padrão dos atributos possíveis.

column_names <- c("Descrição", "Estado Civil", "Grau de Instrução", "Número de Filhos", "Salário", "Ano", "Mês", "Região")
row_names <- c("Média", "Max", "Min", "Moda", "Mediana", "Variância", "Desvio Padrão")

estado_civil <- c(mean(data_db$ESTCIVIL),max(data_db$ESTCIVIL),min(data_db$ESTCIVIL),getmode(data_db$ESTCIVIL),
                  median(data_db$ESTCIVIL),var(data_db$ESTCIVIL),sd(data_db$ESTCIVIL))

instrucao <- c(mean(data_db$INSTRUCAO),max(data_db$INSTRUCAO),min(data_db$INSTRUCAO),getmode(data_db$INSTRUCAO),
               median(data_db$INSTRUCAO),var(data_db$INSTRUCAO),sd(data_db$INSTRUCAO))

filhos <- c(mean(data_db$FILHOS),max(data_db$FILHOS),min(data_db$FILHOS),getmode(data_db$FILHOS),
            median(data_db$FILHOS),var(data_db$FILHOS),sd(data_db$FILHOS))

salario <- c(mean(data_db$SALARIO),max(data_db$SALARIO),min(data_db$SALARIO),getmode(data_db$SALARIO),
             median(data_db$SALARIO),var(data_db$SALARIO),sd(data_db$SALARIO))

ano <- c(mean(data_db$ANO),max(data_db$ANO),min(data_db$ANO),getmode(data_db$ANO),
             median(data_db$ANO),var(data_db$ANO),sd(data_db$ANO))

mes <- c(mean(data_db$MES),max(data_db$MES),min(data_db$MES),getmode(data_db$MES),
         median(data_db$MES),var(data_db$MES),sd(data_db$MES))

regiao <- c(mean(data_db$REGIAO),max(data_db$REGIAO),min(data_db$REGIAO),getmode(data_db$REGIAO),
         median(data_db$REGIAO),var(data_db$REGIAO),sd(data_db$REGIAO))

#---------------------------------------------------------------------------------------------

my_df <- data.frame(row_names)
my_df <- cbind(my_df, estado_civil)
my_df <- cbind(my_df, instrucao)
my_df <- cbind(my_df, filhos)
my_df <- cbind(my_df, salario)
my_df <- cbind(my_df, ano)
my_df <- cbind(my_df, mes)
my_df <- cbind(my_df, regiao)

colnames(my_df) <- column_names

my_df

write.csv(file="preliminary_table.csv", x=my_df)


