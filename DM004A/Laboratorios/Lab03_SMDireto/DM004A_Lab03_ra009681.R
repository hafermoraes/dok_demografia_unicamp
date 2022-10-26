

library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readxl)
library(FactoMineR)
library(factoextra)
library(circlize)


## Pernambuco, Migrações entre 2005 e 2010
## imigrações
imig <- read.table( text = "
municipio	Intrametropolitana	Intraestadual	Interestadual	Internacional
Abreu e Lima	6075	1129	891	22
Araçoiaba	553	231	115	0
Cabo de Santo Agostinho	4314	3860	4498	159
Camaragibe	5941	2132	870	29
Igarassu	4709	1109	1011	9
Ipojuca	2038	2498	1660	67
Ilha de Itamaracá	3808	680	185	44
Itapissuma	1480	325	97	0
Jaboatão dos Guararapes	27584	8437	9641	610
Moreno	1579	882	246	0
Olinda	13750	2883	3042	258
Paulista	20728	3388	2968	133
Recife	17502	20152	25007	1956
São Lourenço da Mata	4905	1582	641	0
",
row.names = 1,
sep = '\t',
header = TRUE,
stringsAsFactors = FALSE
)

## Biplot análise de correspondência
plot( CA(imig) )
fviz_ca_biplot( CA(imig), repel = TRUE )

## Pernambuco, Migrações entre 2005 e 2010
## emigrações
emig <- read.table( text = "
municipio	Intrametropolitana	Intraestadual	Interestadual
Abreu e Lima	3732	567	925
Araçoiaba	436	152	196
Cabo de Santo Agostinho	3392	1428	1675
Camaragibe	5287	1405	776
Igarassu	3048	823	1347
Ipojuca	1482	848	531
Ilha de Itamaracá	741	79	203
Itapissuma	843	124	232
Jaboatão dos Guararapes	12387	3279	5028
Moreno	957	662	367
Olinda	17680	2287	5573
Paulista	9549	1424	2478
Recife	53188	27062	51755
São Lourenço da Mata	2246	941	592
",
row.names = 1,
sep = '\t',
header = TRUE,
stringsAsFactors = FALSE
)

## Biplot análise de correspondência
plot( CA(emig) )
fviz_ca_biplot( CA(emig), repel = TRUE )



## matriz migratória
mmig <- read.table( text = "
municipio	Abreu e Lima	Araçoiaba	Cabo de Santo Agostinho	Camaragibe	Igarassu	Ipojuca	Ilha de Itamaracá	Itapissuma	Jaboatão dos Guararapes	Moreno	Olinda	Paulista	Recife	São Lourenço da Mata
Abreu e Lima	0	71	35	117	993	0	277	161	223	23	362	1071	322	78
Araçoiaba	91	0	6	52	87	0	13	24	0	0	0	54	108	0
Cabo de Santo Agostinho	84	11	0	93	28	437	53	32	1453	125	153	264	647	11
Camaragibe	163	62	71	0	82	31	133	0	871	0	285	378	1491	1718
Igarassu	584	176	84	9	0	37	381	308	264	0	153	691	330	32
Ipojuca	20	0	514	90	29	0	44	9	269	15	33	32	406	19
Ilha de Itamaracá	47	6	74	79	67	0	0	199	36	0	34	103	96	0
Itapissuma	51	20	9	0	372	0	147	0	0	0	27	87	126	5
Jaboatão dos Guararapes	565	5	1719	457	329	371	278	86	0	698	764	1458	5284	373
Moreno	46	0	106	0	78	0	20	0	365	0	24	43	241	34
Olinda	975	78	287	306	875	107	239	224	1443	66	0	7585	5305	191
Paulista	1236	4	109	212	684	111	272	127	1062	49	3180	0	2365	138
Recife	2178	101	1250	4027	1012	910	1900	291	21384	519	8661	8647	0	2306
São Lourenço da Mata	33	21	50	499	73	32	51	19	216	83	75	314	780	0
",
row.names = 1,
sep = '\t',
header = TRUE,
stringsAsFactors = FALSE
)
names(mmig) <- rownames(mmig)

circos.par(start.degree = 90, gap.degree = 12, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
chordDiagram(mmig)
circos.clear()



