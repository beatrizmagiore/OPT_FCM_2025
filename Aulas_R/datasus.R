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


# Aula 30/10

library(tidyverse)

devtools::install_github("danicat/read.dbc")

dados <- read.dbc::read.dbc("Dados/DENGBR24.dbc")

nrow(dados)
names(dados)

# separar municipios de SP (35xxxx)
dados_sp <- dados %>% 
  filter(grepl("^35", ID_MN_RESI))

# remover 'dados' para liberar memória
rm(dados)


# trabalhar com conjunto de dados muito grandes
library(duckdb)

con <- dbConnect(duckdb::duckdb(), dbdir = "Dados/meu_banco.duckdb")
dbWriteTable(con, "dados", dados_sp, overwrite = TRUE)

dbGetQuery(con, "DESCRIBE dados")


##################################################################

# Aula 04/11

library(tidyverse)


roda_arquivo <- function(arquivo){
  dados_simdo <- read.dbc::read.dbc(arquivo)
  
  dados <- dados_simdo %>% 
    select(DTOBITO, CAUSABAS_O) %>% 
    mutate(
      dataobito = as.Date(DTOBITO, "%d%m%Y")
    )
  
  dados <- dados %>% 
    mutate(
      semana = floor(as.integer(dataobito - as.Date("2015-01-01"))/7 + 1),
      diasemana = as.Date("2015-01-01") + 7*(semana-1),
      estado = str_remove(arquivo, ".*DO"),
      estado = str_remove(estado, "\\d{4}\\.dbc")
    )
  
  return(dados)
}

roda_arquivo("Dados/DATASUS/DOAC2015.dbc")

arquivos <- list.files("Dados/DATASUS/", full.names = TRUE)

resultados <- map(arquivos, roda_arquivo)

dados <- Reduce(rbind, resultados)

dados %>%
  group_by(diasemana, estado) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = diasemana, y = n, color = estado))+
  geom_line()+
  theme_minimal()
