### Mini-Projeto 2 - Análise Financeira com Linguagem SQL e Regressão Linear em Linguagem R

# Definindo o Diretório de Trabalho

setwd("C:/Users/marcos/Documents/Cientista_de_Dados/BigDataRAzure/Cap18")
getwd()


# Carregando Bibliotecas
library(tidyverse)
library(RSQLite)
library(ggplot2)
library(sqldf)
library(corrplot)
library(corrgram)
library(randomForest)

# Carregando Banco de Dados
df <- read_csv("dataset.csv")


# Verificando o tipo de Variáveis do dataset
str(df)


# Visualisando o Dataframe e suas dimensões
View(df)
dim(df)


# Função Para verificar Valores Ausêntes
verifica_na <- function(x){
  colSums(is.na(x))
}
verifica_na(df)


# Omitindo Valores NA
df <- na.omit(df)


# Verificando o as dimensões do Dataframe
dim(df)


#### Abaixo as perguntas que devem ser respondidas na análise exploratória com Linguagem SQL: ####

# 1- Quantas raças estão representadas no dataset?
sqldf("select distinct RACE from df")


# 2- Qual a idade média dos pacientes?
sqldf("select avg(AGE) from df")


# 3- Qual a moda da idade dos pacientes?
sqldf("SELECT AGE, count(AGE) as cont
      FROM df
      GROUP BY AGE
      ORDER BY cont desc
      LIMIT 1")


# 4- Qual a variância da coluna idade?
sqldf("select variance(AGE) from df")


# 5 - Qual o gasto total com internações hospitalares por idade?
sqldf("selecT sum(TOTCHG) from df group by AGE")


# 6- Qual idade gera o maior gasto total com internações hospitalares?
sqldf("select AGE, sum(TOTCHG) as soma
      from df
      group by AGE
      order by soma desc
      limit 1")


# 7- Qual o gasto total com internações hospitalares por gênero?
sqldf("select FEMALE, sum(TOTCHG) as soma
      from df
      group by FEMALE
      order by soma desc")


# 8- Qual a média de gasto com internações hospitalares por raça do paciente?
sqldf("select RACE, avg(TOTCHG)
      from df
      group by RACE
      order by RACE")


# 9- Para pacientes acima de 10 anos, qual a média de gasto total com internações
# hospitalares?
sqldf("select AGE, avg(TOTCHG)
      from df
      where AGE > 10
      group by AGE")
      

# 10- Considerando o item anterior, qual idade tem média de gastos superior a 3000?
sqldf("select AGE, avg(TOTCHG) as res
      from df
      where AGE > 10
      group by AGE
      having res > 3000")




#### Abaixo as perguntas que devem ser respondidas na análise estatística: ####

# 1-Qual a distribuição da idade dos pacientes que frequentam o hospital?

# Gerando um Histograma
hist(df$AGE)


# 2- Qual faixa etária tem o maior gasto total no hospital?
faixa_etaria <- df %>%
  select(AGE, TOTCHG) %>%
  group_by(AGE) %>%
  summarise(total_gasto = sum(TOTCHG))
  
# Gerando um  Gráfico de Barras     
barplot(faixa_etaria$total_gasto ~ faixa_etaria$AGE)


# 3- Qual grupo baseado em diagnóstico (Aprdrg) tem o maior gasto total no hospital?

# Obtendo os valores únicos da variável APRDRG
length(unique(df$APRDRG))

diagnostico <- df %>%
  select(TOTCHG, APRDRG) %>%
  group_by(APRDRG) %>%
  summarise(total_gato = sum(TOTCHG)) %>%
  arrange(desc(total_gato))


print(diagnostico[1,])


# 4- A raça do paciente tem relação com o total gasto em internações no hospital?
total_raca <- df%>%
  select(RACE, TOTCHG)%>%
  group_by(RACE)%>%
  summarise(total_gasto = sum(TOTCHG))

# Visualizando o resultado da operação 
View(total_raca)

# Gerando um Gráfico de Barras
barplot(total_raca$total_gasto ~ total_raca$RACE)


# 5- A combinação de idade e gênero dos pacientes influencia no gasto total em internações
# no hospital?
correlacao <- cor(df)
corrplot(correlacao, method = 'square')

# RandomForest com "importance = TRUE" para aferir as relações entre as variáveis
modelo_rf_v1 <- randomForest(TOTCHG ~ AGE + RACE, data = df, nodesize = 10, importance = T)
modelo_rf_v1
summary(modelo_rf_v1)

# Visualizando o resultado do modelo
varImpPlot(modelo_rf_v1)

### Resposta da Pergunta 5:
# As duas variáveis AGE e RACE, juntas apresentam uma significância no valor da internação, sendo
# a variãvel "AGE" mostrando um grau mis forte.


# 6- Como o tempo de permanência é o fator crucial para pacientes internados, desejamos
# descobrir se o tempo de permanência pode ser previsto a partir de idade, gênero e raça.
# Observação: Para responder essa questão vou realizar 3 avalialções.


# Avaliação 1 : Grau de importância com o RandonForest

# Criando um modelo de Arvore de Decisão
modelo_rf_v2 <- randomForest(LOS ~ ., data = df, importance = T)
varImpPlot(modelo_rf_v2)

# Conclusão da Análise acima:

# Na segunda métrica apresentada pelo RandomForest, as variáveis apresentam os menores níveis de
# relevância quando comparandas com as demais variáveis, então nessa primeira análise pode-se concluir
# que o tempo de permanencia não pode ser previsto a partir das três variáveis independentes acima.


# Avaliação 2: Corrgram
corrgram(df)

# Conclusão da Análise acima:

# De acordo com o corrrgram acima, o grau de relacionamento é baixo entre a variável dependente (LOS)
# a as variãveis independetes(AGE, FEMALE, RACE).


# Avaliação 3: Modelo linear
modelo_ln_v1 <- lm(LOS ~ AGE + FEMALE + RACE, data = df)
summary(modelo_ln_v1) # p-value: 0.2692

# Adotando:
# H0: Não há relação entre variáveis
# H1: Há relação entre variáveis

# Conclusão da Análise acima:

# Como o p-value é maior que 0.05 falhamos em rejeitar H0, sendo então que a variável "LOS" não pode
# ser prevista apartir das variáveis "AGE, FEMELE e RACE"


### Resposta da Pergunta 6:
# Não há uma Correlação entre as Variáveis, o tempo de permanência não pode ser previsto apartir 
# das três variáveis. Na última avaliação com o modelo linear, o valor de p foi decisivo para essa 
# conclusão, além de que um outro parâmetro que ajuda a reforçar é a precisão do modelo que está 
# extramamente baixa apenas 3.363


# 7- Quais as variáveis têm  o maior impacto nos custos de internação hospitalar?

# RandomForest com "importance = TRUE" para aferir as relações entre as variáveis
modelo3 <- randomForest(TOTCHG ~ ., data = df, importance = T)
varImpPlot(modelo3)

# Conclusão da Análise acima: 

# Nas duas métricas apresentadas pelo RandomForest, as 3 variáveis com o maior grau de importância 
# são LOS, APRDRG, AGE.

# Modelo linear: Significância
modelo_ln_v2 <- lm(TOTCHG ~ ., data = df)
summary(modelo_ln_v2)

# Conclusão da Análise acima: 

# As variáveis LOS, APRDRG, AGE apresentao um alto level de significância para a previsão dos gastos
# hospitalares.

### Resposta da Pergunta 7: 
### As variáveis com maior impacto no custo de internação, são LOS, APRDRG, AGE.










