library(tidyverse)

# Aula 21/10

dengue_sp <- read.csv2("Dados/dengue_sp.csv")
View(dengue_sp)

names(dengue_sp)

dengue_sp <-  dengue_sp %>% 
  rename(municipio = 1)

dengue_sp <- dengue_sp %>% 
  filter(municipio != "Total",
         municipio != "&",
         !grepl("IGNORADO", municipio))
View(dengue_sp)

# dados do IBGE

dados_ibge <- read.csv2("Dados/tabela4709.csv", skip = 3)
View(dados_ibge)

dados_ibge <- dados_ibge %>% 
  rename(Cod = 1)

## manipulando dados

# separar cod municipio
dengue_sp <- dengue_sp %>% 
  mutate(
    codigo = str_extract(municipio, "\\d{6}")
  )

dados_ibge <- dados_ibge %>% 
  mutate(
    Cod2 = str_extract(Cod, "\\d{6}")
  )

# mudando chr -> int
dengue_sp <- dengue_sp %>% 
  mutate(across(starts_with("x"), as.integer)) %>% 
  replace(is.na(.), 0)

# long table
dengue_sp <- dengue_sp %>% 
  select(-Total) %>% 
  pivot_longer(2:12, names_to = "Ano", values_to = "Casos") %>% 
  mutate(Ano = str_remove_all(Ano, "\\D")) %>% 
  mutate(Ano = as.integer(Ano))

## juntando dados
dados <- dados_ibge %>% 
  select(Cod = Cod2, Inhab = X2022) %>% 
  right_join(dengue_sp, by = c("Cod" = "codigo"))

dados <- dados %>% 
  mutate(
    incidencia = Casos/Inhab*100000
  )

# obs: diferença de usar Casos ou Incidencia
ggplot(dados)+
  geom_boxplot(aes(x = factor(Ano), y = Casos))

ggplot(dados)+
  geom_boxplot(aes(x = factor(Ano), y = incidencia))
