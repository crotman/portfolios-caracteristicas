

library(tidyverse)
library(DBI)
library(lubridate)


conexao <- dbConnect(RSQLite::SQLite(), "dados/db.db")


calcula_armazena_cotas <- function(conexao, caracteristica = "f_score"){
  
  
  dados_caracteristica <- tbl(conexao, "caracteristicas") |>
    filter(
      indice == caracteristica
    ) |> 
    collect() |> 
    mutate(
      data = slu_date_to_r(data)
    ) |> 
    select(
      data,
      acao,
      quintil
    ) |> 
    distinct()
  
  datas_rebalanceamento <- dados_caracteristica |> 
    select(
      data_inicio = data
    ) |> 
    distinct() |> 
    mutate(
      data_fim = lead(data_inicio)
    ) |> 
    replace_na(
      list(
        data_fim = make_date(2100,1,1)
      )
    )
    
    
    precos <- tbl(conexao, "precos") |> 
      collect() |> 
      replace_na(
        list(retorno = 0)
      ) |> 
      mutate(
        data = slu_date_to_r(data)
      ) |> 
      filter(
        data > local(min(datas_rebalanceamento$data_inicio) )
      ) |> 
      left_join(
        datas_rebalanceamento |> rename_with(~str_glue("{.x}_rebal")),
        by = join_by(closest(data > data_inicio_rebal))
      ) |> 
      mutate(
        id = row_number()
      ) |> 
      inner_join(
        dados_caracteristica,
        by = join_by(acao == acao, data_inicio_rebal == data)
      ) |> 
      mutate(
        retorno_acum_acao_no_rebal = cumprod(1 + retorno) - 1 ,
        .by = c(data_inicio_rebal, acao)  
      ) |> 
      summarise(
        .by = c(data_inicio_rebal, data, quintil),
        retorno_acum_portfolio_no_rebal = mean(1 + retorno_acum_acao_no_rebal ) - 1
      ) |> 
      mutate(
        retorno_diario_portfolio = if_else(
          data == first(data),
          retorno_acum_portfolio_no_rebal,
          (1 + retorno_acum_portfolio_no_rebal) / (1 + lag(retorno_acum_portfolio_no_rebal)) - 1
        ),
        .by = c(quintil, data_inicio_rebal )
      ) |> 
      mutate(
        cota_portfolio = cumprod(1 + retorno_diario_portfolio),
        .by = quintil
      ) |> 
      mutate(
        caracteristica = caracteristica  
      )
      

    #dbExecute(conexao, "DELETE FROM RESULTADOS WHERE CARACTERISTICA = '{caracteristica}'" |>  str_glue())

    dbWriteTable(conexao, name = "resultados", precos, append = TRUE)
    
        
}


calcula_armazena_cotas(conexao, "l_p")

calcula_armazena_cotas(conexao, "f_score")





