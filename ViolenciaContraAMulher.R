library (tjsp)
library (tidyverse)
library(pdftools)
library(JurisMiner)
library(tokenizers)


## baixa decisões do tjsp a partir dos parametros informados

baixar_cjsg(
  livre = "",
  aspas = FALSE,
  classe = "417",
  assunto = "14942, 12194, 10949",
  orgao_julgador = "",
  inicio = "",
  fim = "",
  inicio_pb = "",
  fim_pb = "",
  tipo = "A",
  n = NULL,
  diretorio = "."
)


## faz login no site do tjsp

autenticar()


## le o conjunto de julgados em forma de dataframe

cjsg <- tjsp_ler_cjsg(diretorio = "data-raw/cjsg")


## baixa informacoes a respeito de cada um dos julgados no site do tjsp (necessita do login)


tjsp_baixar_cposg(cjsg$processo,diretorio = "data-raw/cposg")


## le cada uma das informacoes do processo e as transforma em dataframe

arquivos <- list.files("data-raw/cposg",full.names = T)

cposg <- ler_dados_cposg(arquivos)

partes <- tjsp_ler_partes(arquivos)                      

dispositivo <- tjsp_ler_dispositivo(arquivos)


## classifica o julgado dos dispositivos a partir de um regex

dispositivo$decisao <- tjsp_classificar_recurso(dispositivo$dispositivo)


##remove colunas que não serão uteis na analise


cjsg$classe = NULL
cjsg$assunto = NULL
cjsg$comarca = NULL
cjsg$data_julgamento= NULL
cjsg$data_publicacao = NULL
dispositivo$data = NULL

## junta o dispositivo e a decisao ao cjsg

analise <- merge(cjsg, dispositivo,
                  by = "processo",
                    all.x = TRUE)


## remove processos em que o MP foi o apelante na tabela partes, pois só queremos analisar as decisões em que o réu foi o apelante

mp_apelante <- partes|>
  filter(tipo_parte == "Apelante:" & parte == "Ministério Público do Estado de São Paulo")

excluir <- merge(analise , mp_apelante,
                 by = "processo",
                 all.x = TRUE)

analise <- excluir |>
  filter(is.na(excluir$parte))
  
analise$tipo_parte = NULL
analise$parte = NULL
analise$representante = NULL

## cria uma tabela com o nome de todos os relatores, para verificar quais são mulheres, após isso classifica entre mulheres e homens

contar <- tibble(cjsg |>
   count(relator))

analise <- analise|>
  mutate(sexo = ifelse(relator == "Angélica de Almeida" | relator == "Claudia Fonseca Fanucchi" | relator == "Claudia Fonseca Fanucchi" | relator == "Ely Amioka" | relator == "Fátima Gomes" | relator == "Fátima Vilas Boas Cruz" | relator == "Gilda  Alves Barbosa  Diodatti" | relator == "Ivana David" | relator == "Maria Tereza do Amaral" | relator == "Vanessa Ribeiro Mateus", "mulher", "homem"))


## remove os processos que correm em segredo de justiça

analise <- analise|>
  filter(dispositivo != is.na(TRUE))

## remove os processos que foram anulados, convertidos em diligencia, não conhecidos ou extintos

analise <- analise|>
  filter(decisao == "improvido"|decisao == "provido"|decisao == "parcial"|decisao == "duvida")

## classificar manualmente os 172 processos em que houve duvida (são os processos em que houveram recursos do MP e do réu. Defini a decisão a respeito do recurso do réu)

analise[157 , 7] <- "parcial"
analise[173 , 7] <- "parcial"
analise[255 , 7] <- "parcial"
analise[345 , 7] <- "provido"
analise[457 , 7] <- "provido"
analise[484 , 7] <- "prejudicado"
analise[535 , 7] <- "provido"
analise[667 , 7] <- "improvido"
analise[719 , 7] <- "improvido"
analise[747 , 7] <- "improvido"
analise[768 , 7] <- "improvido"
analise[783 , 7] <- "improvido"
analise[812 , 7] <- "provido"
analise[877 , 7] <- "provido"
analise[910 , 7] <- "improvido"
analise[947 , 7] <- "improvido"
analise[1021 , 7] <- "provido"
analise[1126 , 7] <- "provido"
analise[1223 , 7] <- "improvido"
analise[1253 , 7] <- "provido"
analise[1324 , 7] <- "parcial"
analise[1325 , 7] <- "parcial"
analise[1527 , 7] <- "parcial"
analise[1559 , 7] <- "improvido"
analise[1561 , 7] <- "improvido"
analise[1637 , 7] <- "improvido"
analise[1640 , 7] <- "improvido"
analise[1655 , 7] <- "provido"
analise[1732 , 7] <- "parcial"
analise[1733 , 7] <- "parcial"
analise[1745 , 7] <- "provido"
analise[1768 , 7] <- "parcial"
analise[1777 , 7] <- "provido"
analise[1783 , 7] <- "provido"
analise[1789 , 7] <- "parcial"
analise[1821 , 7] <- "improvido"
analise[1839 , 7] <- "improvido"
analise[1951 , 7] <- "provido"
analise[2068 , 7] <- "improvido"
analise[2173 , 7] <- "improvido"
analise[2259 , 7] <- "parcial"
analise[2356 , 7] <- "improvido"
analise[2380 , 7] <- "improvido"
analise[2431 , 7] <- "improvido"
analise[2579 , 7] <- "parcial"
analise[2679 , 7] <- "extinto"
analise[2777 , 7] <- "improvido"
analise[2832 , 7] <- "improvido"
analise[2929 , 7] <- "provido"
analise[2935 , 7] <- "parcial"
analise[2968 , 7] <- "extinto"
analise[2991 , 7] <- "improvido"
analise[3027 , 7] <- "improvido"
analise[3123 , 7] <- "improvido"
analise[3156 , 7] <- "parcial"
analise[3162 , 7] <- "extinto"
analise[3190 , 7] <- "improvido"
analise[3194 , 7] <- "improvido"
analise[3250 , 7] <- "improvido"
analise[3280 , 7] <- "improvido"
analise[3287 , 7] <- "parcial"
analise[3303 , 7] <- "improvido"
analise[3318 , 7] <- "improvido"
analise[3462 , 7] <- "parcial"
analise[3573 , 7] <- "parcial"
analise[3578 , 7] <- "improvido"
analise[3583 , 7] <- "parcial"
analise[3609 , 7] <- "prejudicado"
analise[3677 , 7] <- "provido"
analise[3725 , 7] <- "parcial"
analise[3741 , 7] <- "improvido"
analise[3780 , 7] <- "parcial"
analise[3809 , 7] <- "provido"
analise[3883 , 7] <- "improvido"
analise[3908 , 7] <- "parcial"
analise[3918 , 7] <- "improvido"
analise[3919 , 7] <- "improvido"
analise[3953 , 7] <- "improvido"
analise[3957 , 7] <- "parcial"
analise[3964 , 7] <- "improvido"
analise[4000 , 7] <- "improvido"
analise[4006 , 7] <- "improvido"
analise[4042 , 7] <- "improvido"
analise[4043 , 7] <- "improvido"
analise[4081 , 7] <- "improvido"
analise[4101 , 7] <- "improvido"
analise[4105 , 7] <- "improvido"
analise[4125 , 7] <- "improvido"
analise[4135 , 7] <- "improvido"
analise[4164 , 7] <- "parcial"
analise[4165 , 7] <- "improvido"
analise[4166 , 7] <- "improvido"
analise[4243 , 7] <- "improvido"
analise[4268 , 7] <- "improvido"
analise[4272 , 7] <- "improvido"
analise[4327 , 7] <- "provido"
analise[4364 , 7] <- "provido"
analise[4393 , 7] <- "improvido"
analise[4558 , 7] <- "improvido"
analise[4563 , 7] <- "provido"
analise[4630 , 7] <- "provido"
analise[4792 , 7] <- "improvido"
analise[4901 , 7] <- "improvido"
analise[4916 , 7] <- "improvido"
analise[5057 , 7] <- "parcial"
analise[5098 , 7] <- "provido"
analise[5103 , 7] <- "parcial"
analise[5133 , 7] <- "improvido"
analise[5141 , 7] <- "improvido"
analise[5143 , 7] <- "improvido"
analise[5175 , 7] <- "improvido"
analise[5250 , 7] <- "improvido"
analise[5289 , 7] <- "improvido"
analise[5316 , 7] <- "improvido"
analise[5352 , 7] <- "parcial"
analise[5457 , 7] <- "provido"
analise[5727 , 7] <- "improvido"
analise[5772 , 7] <- "improvido"
analise[5782 , 7] <- "parcial"
analise[5784 , 7] <- "parcial"
analise[5816 , 7] <- "improvido"
analise[5828 , 7] <- "improvido"
analise[5918 , 7] <- "improvido"
analise[6004 , 7] <- "improvido"
analise[6105 , 7] <- "improvido"
analise[6145 , 7] <- "parcial"
analise[6241 , 7] <- "parcial"
analise[6290 , 7] <- "provido"
analise[6315 , 7] <- "provido"
analise[6366 , 7] <- "improvido"
analise[6541 , 7] <- "parcial"
analise[6649 , 7] <- "improvido"
analise[6691 , 7] <- "parcial"
analise[6693 , 7] <- "improvido"
analise[6702 , 7] <- "improvido"
analise[6761 , 7] <- "parcial"
analise[6995 , 7] <- "improvido"
analise[7080 , 7] <- "improvido"
analise[7209 , 7] <- "improvido"
analise[7230 , 7] <- "improvido"
analise[7443 , 7] <- "improvido"
analise[7447 , 7] <- "provido"
analise[7511 , 7] <- "provido"
analise[7520 , 7] <- "improvido"
analise[7540 , 7] <- "improvido"
analise[7590 , 7] <- "improvido"
analise[7801 , 7] <- "parcial"
analise[7829 , 7] <- "improvido"
analise[7863 , 7] <- "improvido"
analise[7896 , 7] <- "improvido"
analise[7899 , 7] <- "parcial"
analise[8033 , 7] <- "improvido"
analise[8034 , 7] <- "improvido"
analise[8158 , 7] <- "parcial"
analise[8172 , 7] <- "improvido"
analise[8173 , 7] <- "improvido"
analise[8216 , 7] <- "improvido"
analise[8249 , 7] <- "improvido"
analise[8307 , 7] <- "parcial"
analise[8319 , 7] <- "improvido"
analise[8362 , 7] <- "improvido"
analise[8465 , 7] <- "parcial"
analise[8475 , 7] <- "provido"
analise[8505 , 7] <- "improvido"
analise[8519 , 7] <- "improvido"
analise[8534 , 7] <- "improvido"
analise[8613 , 7] <- "improvido"
analise[8630 , 7] <- "improvido"
analise[8649 , 7] <- "improvido"
analise[8717 , 7] <- "improvido"
analise[8759 , 7] <- "improvido"
analise[8799 , 7] <- "improvido"
analise[8831 , 7] <- "improvido"

analise <- analise|>
  filter(decisao == "improvido"|decisao == "provido"|decisao == "parcial")

## remove processos iguais

analise <- analise|>
  distinct(processo, .keep_all = T)


## separa as decisoes entre favoráveis ou contrárias ao réu

analise <- analise|>
  mutate(favoravel_ao_reu = ifelse(decisao == "improvido", "nao", "sim"))

## simplifica os nomes das camaras

analise <- analise |>
  mutate(camara = str_remove_all(orgao_julgador, "[[:lower:]|\\s]+"))

analise$orgao_julgador = NULL


## define se o advogado da parte é defensor público ou não

defensoria <- partes|>
  mutate(defensoria = ifelse(str_detect(representante, "(?i)defensor"),"defensoria","particular"))

defensoria$tipo_parte = NULL
defensoria$parte = NULL
defensoria$representante = NULL

defensoria <- defensoria |>
  distinct(processo, .keep_all = T)

analise <- merge(defensoria, analise,
                 by = "processo",
                 all.x = TRUE)

analise <- analise|>
  mutate(defensoria = ifelse(is.na(defensoria), "particular", analise$defensoria))


## baixa os acordaos de todos os processos analisados e os lista


acordaos <- tjsp_baixar_acordaos_cjsg(analise$cdacordao, diretorio = "data-raw/acordaos")

corpo <- corpus(acordaos, docid_field = "cdacordao",
                text_field = "julgado")

arquivos_acordao = list.files(pattern = 'cdacordao', recursive = TRUE)

acordaos <- tjsp_ler_acordaos_cjsg(arquivos_acordao)


## busca trechos no acordao que falam a respeito dos antecedentes do réu e classifica como bons antecedentes ou maus antecedentes

tokens_antecedentes <- jus_kwic(acordaos$julgado, nomes = acordaos$cdacordao,
                   regex = "(?i)(antecedentes|prim[aá]ri|favor[aá]v|reincid)",
                   tbl = TRUE)
tokens_antecedentes <- tokens_antecedentes |>
  mutate(classe = classifica_antecedentes(trecho))

tokens_antecedentes <- tokens_antecedentes|>
  filter(classe != is.na(TRUE))

tokens_antecedentes$classe = ifelse(tokens_antecedentes$classe == "0", "maus", "bons")


antecedentes <- tokens_antecedentes|>
  group_by(id)|>
  summarise(id = paste(id, collapse = ", "), 
            trecho = paste(trecho, collapse = ", "), 
            classe = paste(classe, collapse = ", "))

id = antecedentes$id


id <- str_replace(id, "(?<=,)(.*)", "")

id <- str_replace(id, "([,])", "")

antecedentes$id <- id

antecedentes <- rename(antecedentes, cdacordao = id)

analise <- merge(analise, antecedentes,
                  by = "cdacordao",
                  all.x = TRUE)
analise$trecho = NULL

analise <- analise|>
  mutate(classe = ifelse(is.na(analise$classe), "desconhecido", analise$classe))




## busca trechos no acordao que falam a respeito de uso de armas de qualquer tipo

tokens_armas <- jus_kwic(acordaos$julgado, nomes = acordaos$cdacordao,
                         regex = "(?i)(arma|faca|pistola|cortante|martelo|rev.lver)",
                         tbl = TRUE)

armas <- tokens_armas|>
  mutate(armas = "armado")

armas$trecho = NULL

armas <- armas|>
  distinct(id, .keep_all = T)


armas <- rename(armas, cdacordao = id)

analise <- merge(analise, armas,
                  by = "cdacordao",
                  all.x = TRUE)

analise <- analise|>
  mutate(armas = ifelse(is.na(analise$armas), "desconhecido", analise$armas))


saveRDS(analise, "analise.RDS")



