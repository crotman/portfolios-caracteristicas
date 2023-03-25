
library(tidyverse)
library(readxl)
library(janitor)
library(DBI)
library(sqliteutils)

limpa_caracteristica <- function(dados, coluna_inicial){
  
  print(coluna_inicial)
  

  dados_colunas <- dados |> 
    select(coluna_inicial:(coluna_inicial + 2))
  
  data <- dados_colunas[1,1] |> as.integer() |>  excel_numeric_to_date()
  
  saida <-  dados_colunas |> 
    row_to_names(row_number = 2) |> 
    clean_names() |> 
    rename(
      quintil = 3
    ) |> 
    mutate(
      data = data
    ) |> 
    mutate(
      across(
        .cols = c(2,3),
        .fns = as.numeric
      )
    ) |> 
    filter(
      !is.na(acao)
    )

  saida |> 
    mutate(
      indice = names(saida)[2]
    ) |> 
    rename(
      valor = 2
    )
  
}


le_sheet_caracteristica <- function(arquivo = "dados-brutos/Consolidação de resultados 1T.xlsx", sheet = "LP"){
  
  dados_caracteristica <- read_excel(arquivo, sheet = sheet, col_names = FALSE)

  colunas_iniciais <- seq(from = 1, length(dados_caracteristica), by = 3)
  
  saida <- map_df(
    .x = colunas_iniciais,
    .f = ~limpa_caracteristica(dados = dados_caracteristica, coluna_inicial = .x)
  )
    
}


arquivo <-  "dados-brutos/Consolidação de resultados 1T.xlsx"

sheets <- excel_sheets(arquivo_resultados)

dados_caracteristicas <- 
  map_df(
    .x = sheets,
    .f = ~le_sheet_caracteristica(arquivo = arquivo, sheet = .x)  
  )
  
dados_precos <- read_excel("dados-brutos/Prices Dissertation.xlsx") |> 
  pivot_longer(
    cols = -Data,
    names_to = "acao",
    values_to = "preco"
  ) |> 
  mutate(
    preco = as.numeric(preco)
  ) |> 
  clean_names() |> 
  group_by(
    acao
  ) |> 
  fill(
    preco,
    .direction = "down"
  ) |> 
  ungroup() |> 
  mutate(
    data = as.Date(data)
  ) |> 
  mutate(
    retorno = preco/lag(preco) -1,
    .by = acao    
  ) 
  

conexao <- dbConnect(RSQLite::SQLite(),  "dados/db.db")

dbWriteTable(conn = conexao, name = "caracteristicas", value = dados_caracteristicas |> distinct(), overwrite = TRUE )

dbWriteTable(conn = conexao, name = "precos", value = dados_precos, overwrite = TRUE )

dbDisconnect(conexao)


