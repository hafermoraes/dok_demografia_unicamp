
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
        idade_orig = D6N,
        nDx = as.numeric(V)
    ) %>%
    mutate(
        idade_orig = case_when(
            idade_orig == '90 a 94 anos' ~ '90 anos ou mais',
            idade_orig == '95 a 99 anos' ~ '90 anos ou mais',
            idade_orig == '100 anos ou mais' ~ '90 anos ou mais',
            TRUE ~ as.character(idade_orig)
        )
    ) %>%
    group_by( fonte, sexo, idade_orig ) %>%
    summarise( nDx = sum(nDx) )

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
        idade_orig = D7N,
        nDx = as.numeric(V)
    ) %>%
    mutate(
        idade_orig = case_when(
            idade_orig == '90 a 94 anos' ~ '90 anos ou mais',
            idade_orig == '95 a 99 anos' ~ '90 anos ou mais',
            idade_orig == '100 anos ou mais' ~ '90 anos ou mais',
            TRUE ~ as.character(idade_orig)
        )
    ) %>%
    group_by( fonte, sexo, idade_orig ) %>%
    summarise( nDx = sum(nDx) )

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
        idade_orig = cut(
            x = idade_aux,
            breaks = c(0,1,seq(from=5,to=90,by=5),150),
            labels = c('Menos de 1 ano', '1 a 4 anos', '5 a 9 anos', '10 a 14 anos',
                       '15 a 19 anos','20 a 24 anos','25 a 29 anos','30 a 34 anos',
                       '35 a 39 anos','40 a 44 anos','45 a 49 anos','50 a 54 anos',
                       '55 a 59 anos','60 a 64 anos','65 a 69 anos','70 a 74 anos',
                       '75 a 79 anos','80 a 84 anos','85 a 89 anos','90 anos ou mais'),
            include.lowest = TRUE,
            right = FALSE
        )
    ) %>%
    group_by( fonte, sexo, idade_orig ) %>%
    summarise( nDx = n() ) %>%
    na.omit() 

## Tábua de vida masculina de Pernambuco em 2010
tvpe2010_male <- tribble(
    ~idade_orig, ~populacao, ~nDx, ~mxn, ~x, ~qxn, ~lx, ~dxn, ~Lxn, ~Tx, ~ex,
    "Menos de 1 ano",66758,1427,0.02138,0,0.02098,100000,2098,98118,6682774,66.8,
    "1 a 4 anos",277508,227,0.00082,1,0.00327,97902,320,390838,6584656,67.3,
    "5 a 9 anos",378324,123,0.00033,5,0.00162,97582,159,487516,6193818,63.5,
    "10 a 14 anos",423568,238,0.00056,10,0.00281,97424,273,486436,5706302,58.6,
    "15 a 19 anos",407498,1042,0.00256,15,0.01271,97150,1234,482666,5219866,53.7,
    "20 a 24 anos",402836,1693,0.0042,20,0.02079,95916,1994,474594,4737200,49.4,
    "25 a 29 anos",379000,1530,0.00404,25,0.01998,93922,1877,464916,4262606,45.4,
    "30 a 34 anos",344709,1526,0.00443,30,0.02189,92045,2015,455187,3797690,41.3,
    "35 a 39 anos",301541,1468,0.00487,35,0.02405,90030,2165,444737,3342503,37.1,
    "40 a 44 anos",271173,1675,0.00618,40,0.03041,87865,2672,432643,2897766,33,
    "45 a 49 anos",233862,1985,0.00849,45,0.04157,85193,3541,417110,2465122,28.9,
    "50 a 54 anos",191000,2066,0.01082,50,0.05266,81652,4300,397508,2048012,25.1,
    "55 a 59 anos",152743,2350,0.01539,55,0.07408,77352,5730,372432,1650504,21.3,
    "60 a 64 anos",128560,2780,0.02163,60,0.10258,71621,7347,339738,1278072,17.8,
    "65 a 69 anos",95597,3094,0.03237,65,0.14973,64274,9624,297311,938334,14.6,
    "70 a 74 anos",73653,3568,0.04845,70,0.21607,54650,11808,243730,641023,11.7,
    "75 a 79 anos",46054,3218,0.06988,75,0.29742,42842,12742,182353,397293,9.3,
    "80 a 84 anos",31232,3208,0.10272,80,0.40864,30100,12300,119748,214940,7.1,
    "85 a 89 anos",16348,2560,0.15658,85,0.56264,17800,10015,63961,95192,5.4,
    "90 anos ou mais",8717,2173,0.24926,90,1,7785,7785,31231,31231,4
)

## Tábua de vida feminina de Pernambuco em 2010
tvpe2010_female <- tribble(
    ~idade_orig, ~populacao, ~nDx, ~mxn, ~x, ~qxn, ~lx, ~dxn, ~Lxn, ~Tx, ~ex,
    "Menos de 1 ano",64528,1041,0.01614,0,0.01591,100000,1591,98565,7546599,75.5,
    "1 a 4 anos",268115,194,0.00072,1,0.00289,98409,285,392926,7448034,75.7,
    "5 a 9 anos",366005,97,0.00027,5,0.00133,98125,130,490299,7055108,71.9,
    "10 a 14 anos",411963,140,0.00034,10,0.0017,97995,167,489556,6564809,67,
    "15 a 19 anos",406100,230,0.00057,15,0.00282,97828,276,488449,6075253,62.1,
    "20 a 24 anos",414746,306,0.00074,20,0.00369,97552,360,486859,5586804,57.3,
    "25 a 29 anos",400641,369,0.00092,25,0.0046,97192,447,484843,5099946,52.5,
    "30 a 34 anos",372344,447,0.0012,30,0.00598,96745,578,482280,4615103,47.7,
    "35 a 39 anos",333661,565,0.00169,35,0.00843,96167,811,478807,4132822,43,
    "40 a 44 anos",305896,774,0.00253,40,0.01257,95356,1198,473783,3654016,38.3,
    "45 a 49 anos",268313,1017,0.00379,45,0.01878,94157,1768,466367,3180233,33.8,
    "50 a 54 anos",225663,1316,0.00583,50,0.02875,92389,2656,455306,2713866,29.4,
    "55 a 59 anos",190010,1589,0.00836,55,0.04095,89733,3674,439480,2258560,25.2,
    "60 a 64 anos",160049,2089,0.01305,60,0.0632,86059,5439,416697,1819080,21.1,
    "65 a 69 anos",124093,2479,0.01998,65,0.09514,80620,7671,383924,1402383,17.4,
    "70 a 74 anos",100594,3224,0.03205,70,0.14836,72950,10823,337690,1018459,14,
    "75 a 79 anos",66426,3229,0.0486,75,0.21669,62127,13462,276977,680768,11,
    "80 a 84 anos",46240,3690,0.07981,80,0.33267,48664,16189,202849,403791,8.3,
    "85 a 89 anos",24574,3051,0.12416,85,0.47374,32475,15385,123915,200942,6.2,
    "90 anos ou mais",15806,3507,0.22188,90,1,17091,17091,77028,77028,4.5
)

labels <- c('<1','1-4','5-9','10-14','15-19','20-24','25-29',
            '30-34','35-39','40-44','45-49','50-54','55-59',
            '60-64','65-69','70-74','75-79','80-84','85-89',
            '90+'
            )

tvpe2010_female <- bind_cols( tvpe2010_female, idade = labels )
tvpe2010_male <- bind_cols( tvpe2010_male, idade = labels)

dat_nmx <- bind_rows( pe2010_censo, pe2010_rc, pe2020_sim ) %>% # nDx: óbitos por sexo e grupo etário quinquenal
    left_join(
        bind_rows( # nNx: população por sexo e grupo quinquenal de idade até 90+
            tvpe2010_female %>% transmute( sexo = 'Feminino', idade_orig, idade, nNx = populacao ) , 
            tvpe2010_male %>% transmute( sexo = 'Masculino', idade_orig, idade, nNx = populacao )
        )
    ) %>%
    mutate(
        idade = factor( idade, levels = labels ),
        nmx = nDx / nNx,
        log_nmx = log(nmx)
    )

plot1 <- dat_nmx %>%
    ggplot( aes( x = idade, y = log_nmx, color = fonte, group = fonte ) ) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(
        x = 'faixa etária',
        y = 'logaritmo da taxa específica de mortalidade',
        color = 'Fonte dos óbitos (nDx)',
        group = ''
    ) + 
    theme(legend.position = "top", axis.text.x = element_text(angle = 90)) +
    facet_wrap( ~ sexo  , scales = 'free')

ggsave(filename = '../img/lab01_ex01.png', plot = plot1, width = 10, height = 4)

