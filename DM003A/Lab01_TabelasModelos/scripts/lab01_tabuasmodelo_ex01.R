
library(RCurl)         # getURL
library(jsonlite)      # fromJSOn
library(dplyr)         # mutate, %>% ,select, filter, transmute, etc...
library(stringr)       # str_detect
library(ggplot2)       # ggplot, geom_line, facet_wrap, ...
library(microdatasus)  # fetch_datasus, process_sim

## Óbitos provenientes do Censo 2010
pe2010_censo <- RCurl::getURL(                     # baixa o conteúdo da URL em formato JSON
           url = 'https://apisidra.ibge.gov.br/values/t/3223/n3/26/v/allxp/p/all/c462/0/c321/all/c259/0,5898,5899,5900,5901,5902,5903,5904,5905,5906,5907,5908,5909,5910,5911,5912,5913,5914,5915,5917,5918,5919,5920,12091'
       ) %>%   
    jsonlite::fromJSON() %>%                        # converte formato de JSON para data frame
    tibble() %>%                                    # converte formato de data frame para tibble
    filter(
        V != 'Valor', V != '-', D5N != 'Total'      # ignora registros com '-', 'Valor' na coluna V e 'Total' na coluna sexo
       ,str_detect( D6N, '\\d')                     # somente faixas etárias quinquenais
    ) %>%
    transmute(
        fonte = 'censo',
        sexo = ifelse( D5N == 'Homens', 'Masculino', 'Feminino'),
        idade = D6N,
        obitos = as.numeric(V)
    )

## Óbitos provenientes do Registro Civil
pe2010_rc <- RCurl::getURL(                     # baixa o conteúdo da URL em formato JSON
           url = 'https://apisidra.ibge.gov.br/values/t/2682/n3/26/v/allxp/p/2010/c255/12030/c1836/0/c2/all/c260/0,5922,5948,5953,5959,5966,5967,5968,5969,5970,5971,5972,5973,5974,5975,5976,5977,5978,5979,5996,106181,106182,106183/c257/0'
       ) %>%   
    jsonlite::fromJSON() %>%                        # converte formato de JSON para data frame
    tibble() %>%                                    # converte formato de data frame para tibble
    select( D6N, D7N, V) %>%
    filter(
        V != 'Valor', V != '-', D6N != 'Total'      # ignora registros com '-', 'Valor' na coluna V e 'Total' na coluna sexo
       ,str_detect( D7N, '\\d')                     # somente faixas etárias quinquenais
    ) %>%
    transmute(
        fonte = 'registro civil',
        sexo = ifelse( D6N == 'Homens', 'Masculino', 'Feminino'),
        idade = D7N,
        obitos = as.numeric(V)
    ) 

## Óbitos provenientes do SIM/Datasus
sim <- fetch_datasus(
    year_start = 2010
   ,year_end = 2010
   ,uf = 'PE'
   ,information_system = "SIM-DO" ) %>%
    filter( IDADE != '000', IDADE != '999' )

sim <- process_sim(sim  )

pe2020_sim <- sim %>%
    mutate(
        fonte = 'SIM',
        sexo = SEXO,
        idade_aux = case_when(
            !is.na(IDADEanos) ~ IDADEanos,
            TRUE ~ 0
        ),
        idade = cut(
            x = idade_aux,
            breaks = c(0,1,seq(from=5,to=100,by=5),150),
            labels = c('Menos de 1 ano', '1 a 4 anos', '5 a 9 anos', '10 a 14 anos',
                       '15 a 19 anos','20 a 24 anos','25 a 29 anos','30 a 34 anos',
                       '35 a 39 anos','40 a 44 anos','45 a 49 anos','50 a 54 anos',
                       '55 a 59 anos','60 a 64 anos','65 a 69 anos','70 a 74 anos',
                       '75 a 79 anos','80 a 84 anos','85 a 89 anos','90 a 94 anos',
                       '95 a 99 anos','100 anos ou mais'),
            include.lowest = TRUE,
            right = FALSE
        )
    ) %>%
    count( fonte, sexo, idade ) %>%
    rename( obitos = n) %>%
    na.omit() 

bind_rows( pe2010_censo, pe2010_rc, pe2020_sim ) %>%
    mutate( idade = factor( idade, levels = c('Menos de 1 ano', '1 a 4 anos', '5 a 9 anos', '10 a 14 anos',
                       '15 a 19 anos','20 a 24 anos','25 a 29 anos','30 a 34 anos',
                       '35 a 39 anos','40 a 44 anos','45 a 49 anos','50 a 54 anos',
                       '55 a 59 anos','60 a 64 anos','65 a 69 anos','70 a 74 anos',
                       '75 a 79 anos','80 a 84 anos','85 a 89 anos','90 a 94 anos',
                       '95 a 99 anos','100 anos ou mais') )
           ) %>%
    ggplot( aes( x = idade, y = obitos, color = fonte, group = fonte ) ) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(
        #title = 'Comparativo do número de óbitos por sexo para Pernambuco em 2010',
        #subtitle = 'Censo vs Registro Civil vs Datasus/SIM',
        x = 'faixa etária',
        y = 'número de óbitos'
        #caption = 'Fontes: Censo - Tabela 3223/SIDRA, Registro Civil - Tabela 2682 e SIM - Pacote "microdatasus" do R. Acesso em 12/10/2022. Elaboração própria.'
    ) + 
    facet_wrap( ~sexo, ncol=2 )
