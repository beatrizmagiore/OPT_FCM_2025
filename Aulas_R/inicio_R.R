library(tidyverse)

# aula 14/10
dados <- read.csv("Dados/Pokemon_full.csv")

head(dados)
names(dados)

select(dados, name, hp)
filter(dados, attack > 50)

mutate(dados, x = attack+speed)
mutate(dados, IMC = weight/(height*height))

dados <- mutate(dados, IMC = weight/(height*height))

dados %>% 
  select(name, hp, attack, speed) %>% 
  filter(attack < 50) %>% 
  mutate(x = attack+speed)

# Aula 16/10
dados %>% 
  filter(height >10) %>% 
  select(name, height, weight) %>% 
  mutate(IMC = weight/(height*height)) %>% 
  ggplot()+
  geom_density(aes(x=IMC))

glimpse(dados)

dados %>% pull(IMC) # retorna como vetor

dados %>% 
  group_by(type) %>% 
  summarise(media = mean(IMC),
            desvio = sd(IMC))

# buscar padrões em strings
## aceita Regular Expressions (ReGex)
grep("saur|fly", dados$name) #retorna vetor com os números das posições
grepl("saur", dados$name) # retorna vetor do mesmo tamanho com TRUE/FALSE
grep("[Ss]aur", dados$name)

dados %>% 
  filter(grepl("saur|fly", name))

## juntando dados

# colunas iguais
df1 <- dados %>% 
  filter(attack > 70)

df2 <- dados %>% 
  filter(attack <= 70)

rbind(df1,df2) #juntar linhas

# colunas diferentes
df1 <- dados %>%
  select(attack, speed, weight) %>% 
  filter(attack > 70)

df2 <- dados %>% 
  select(attack, weight, height, hp) %>% 
  filter(attack <= 70)

bind_rows(df1,df2) # completa ausências com NA

# juntar colunas
df1 <- dados %>% head(100)
df2 <- dados %>% tail(100)

bind_cols(df1,df2)

# join
## left: mantém da esquerda com NA e dispensa da direita
## right: mantém da direita com NA e dispensa da esquerda
## full: mantém tudo com NA
## inner: dispensa tudo

df_resumo <- dados %>% 
  group_by(type) %>% 
  summarise(media = mean(IMC),
            desvio = sd(IMC))

left_join(dados, df_resumo, by = "type")

df_resumo_mis <- df_resumo %>% filter(type != "grass")
left_join(dados, df_resumo_mis, by = "type") %>% View()
right_join(dados, df_resumo_mis, by = "type") %>% View()

# Aula 21/10