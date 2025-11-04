#' coleta de dados via API SIDRA - IBGE
#'
#' Esta função retorna a tabela solicitada em formato data.frame.
#' @param tabela Número da tabela.
#' @param classificador Classificador a ser detalhado. O padrão é "", retornando os totais da tabela. Para verificar os classificadores disponíveis na tabela em questão use a função tab_class().
#' @param filtro_cats Código para definição de subconjunto do classificador. Para verificar as categorias disponíveis na tabela em questão use a função tab_class().
#' @param nivel Nível geográfico de agregação dos dados 1 = Brasil e 6 = Município, etc. Para verificar os níveis disponíveis na tabela em questão use a função tab_niveis().
#' @param filtro_niveis Código contendo conjunto no nível que será selecionado. Pode-se usar o código de determina UF para obter apenas seus dados ou "all" para todos (padrão). Para mais informações visite http://api.sidra.ibge.gov.br/home/ajuda.
#' @param periodo Período dos dados. O padrão é "all", isto é, todos os anos disponíveis. Para verificar os períodos disponíveis na tabela em questão use a função tab_periodos().
#' @param variavel Quais variáveis devem retornar? O padrão é "allxp", isto é, todas exceto aquelas calculadas pela SIDRA (percentuais). Para verificar as variáveis disponíveis na tabela em questão use a função tab_vars().
#' @param inicio,fim Início e fim do período desejado.
#' @param part interno para quando é preciso fazer várias requisições
#' @param printurl imprime url construído para transparência e debugging
#' @return Um `data.frame` (`tibble`) contendo os dados solicitados da tabela SIDRA. A estrutura do \code{data.frame} está em formato amplo (wide),
#' onde cada linha geralmente corresponde a um nível geográfico e período de tempo específico. As colunas incluem detalhes sobre a localidade, o período e as variáveis,
#' com as categorias de classificação transformadas em colunas separadas.
#' @keywords IBGE SIDRA dados
#' @export
#' @examples
#' ipcaq <- sidra(1705,classificador=315,periodo='201202')

sidra <- function (tabela, classificador="",
                   filtro_cats ="", nivel = "N1",
                   filtro_niveis,
                   periodo =
                     tab_meta(tabela)$periodos, variavel = "all",
                   inicio, fim,part=FALSE,printurl=FALSE)
{
  if (length(tabela) > 1) {
    stop("Solicite os dados de uma tabela por vez. Para mais de uma use fun\u00e7\u00f5es da fam\u00edlia apply",
         call. = FALSE)
  }
  if (!tabela %in% sidra::sidrameta$id) {
    stop("A tabela informada n\u00e3o \u00e9 v\u00e1lida", call. = FALSE)
  }

  ##obtendo metadados única vez
  metatab <- tab_meta(tabela)
  if (is.null(metatab)){
    return(invisible(NULL))
  }

  #Determinação do período de forma vetorizada
  periodo <-   if (!missing(inicio) && !missing(fim)) {
    paste0(inicio, "-", fim)
  } else if (missing(fim) && !missing(inicio)) {
    metatab$periodos[metatab$periodos >= inicio]
  } else if (missing(inicio) && !missing(fim)) {
    metatab$periodos[metatab$periodos <= fim]
  } else {
    periodo
  }

  #Validando e ajustando `filtro_niveis` e `filtro_cats`
  if (!missing(filtro_niveis) && length(nivel) != length(filtro_niveis)) {
    stop("O argumento filtro_niveis, quando especificado, deve ser uma lista de
         mesmo tamanho que o argumento n\u00edvel",
         call. = FALSE)
  }

  if (!missing(filtro_cats) && !missing(classificador) &&
      length(classificador) != length(filtro_cats)) {
    stop("O argumento filtro_cats, quando especificado,
         deve ser uma lista de mesmo tamanho que o argumento classificador",
         call. = FALSE)
  }

  # Função interna para concatenar elementos
  concav <- \(x) paste0(x,collapse=",")

  # Preparação de classificadores e localidades
  classifs <- if(!missing(filtro_cats) & !missing(classificador) & length(classificador)>0) {
    paste0(paste0(classificador,"[",sapply(filtro_cats,concav),"]"),collapse="|")
  } else if(!missing(classificador) & length(classificador)!=0){
    paste0(classificador,"[all]",collapse="|")
  } else {
    paste0(paste0(gsub("[^[:digit:]]","",names(metatab$classificacoes)),
                  collapse="[all],"),
           "[all]")
  }

  locais <- if(!missing(filtro_niveis)) {
    paste0(paste0(nivel,"[",sapply(filtro_niveis,concav),"]"),collapse="|")
  } else {
    paste0(paste0(nivel,collapse="[all]|"),"[all]")
  }

  # Concatenando variáveis de forma otimizada
  qtdvar <- length(variavel)
  variavel <-
  if(length(variavel)>1) {
    paste0(variavel,collapse="|")
  } else {variavel}



  # C\u00e1lculo do tamanho da requisi\u00e7\u00e3o:

  ntemps <- if(is.character(periodo) && !grepl("-",periodo[1]) & length(periodo>1)) {
    length(periodo)
    } else if (grepl("-",periodo[1])) {
    length(metatab$periodos[metatab$periodos >=substr(periodo[1],1,6) & metatab$periodos<=substr(periodo[1],8,nchar(periodo))])
  } else if (grepl("|",periodo[1])){
    length(strsplit(as.character(periodo),"|",fixed = T))
  } else {
    length(metatab$periodos)
  }
  periodo <- paste0(periodo,collapse="|")
  # Definindo ncats
  ncats <- if(!missing(filtro_cats)){
    sum(sapply(filtro_cats,length),na.rm=T)
  } else {
    class_esc <-
    if(classificador!="") {
      metatab$classificacoes[grepl(paste0("^",classificador,"-"),names(metatab$classificacoes))]
      } else {
        metatab$classificacoes
      }

    sum(sapply(class_esc,nrow),na.rm=T)
  }

  #Definindo nlocs
  nlocs <-
  if(!missing(filtro_niveis)){
    sum(sapply(filtro_niveis,length),na.rm=T)
  } else {
    nvl <- tab_niveis(tabela)
    nlocs <- nrow(nvl[nvl$nivel.id %in% nivel])
  }

  nvars <-{
    if(qtdvar>1){
      qtdvar
    } else if(variavel=="all"){
      nrow(metatab$variaveis)
    } else {1}
  }

  # Calculando tamanho da consulta e particionando se necessário
  tamanho <- ifelse(part==T,0,nvars*ntemps*nlocs*ncats)
  #Construindo URL
  base_url <- "https://servicodados.ibge.gov.br/api/v3/agregados/"
  url <- gsub("\\[\\]","",paste0(base_url, tabela,
                                 "/periodos/", periodo,
                                 "/variaveis/", variavel,
                                 "?classificacao=", classifs,
                                 "&localidades=", locais))

  if(printurl){print(url)}

  if (tamanho>1e5) {
    message(paste(
      "A consulta exceder\u00e1 o limite de 100.000 pontos de dados permitido pela API.",
      "Vamos contornar este problema fazendo v\u00e1rias solicita\u00e7\u00f5es menores.",
      "Haver\u00e1 maior demora", sep = "\n"))

    periodos <- metatab$periodos
    requisicoes <- (tamanho %/% 100000) + 3

    cada <- periodos |> split(cut(seq_along(periodos), requisicoes)) |>
      lapply(range) |> sapply(paste0, collapse = "-")

  fnvl <- ifelse(missing(filtro_niveis),"",filtro_niveis)
  fcats <- ifelse(missing(filtro_cats),"",filtro_cats)

    res <- lapply(cada, \(x) {sidra(
           tabela = tabela, classificador = classifs,
           filtro_cats = filtro_cats, nivel = nivel,
           filtro_niveis = fnvl,
           periodo= x,
           variavel = variavel,part=T,printurl=printurl)})
    res <- data.table::rbindlist(res)
    return(res)
  }


  # Fazer a requisi\u00e7\u00e3o GET
  response <- call_ibge({httr::GET(url,config = httr::timeout(2))})

  # verificação do conteúdo
  # Checar se a requisição foi bem-sucedida
  if (is.null(response)){
    warning("Sem acesso conseguido a URL da API do IBGE.")
    return(invisible(data.table::data.table()))
  }


  # Converter o JSON para um data frame
  json_data <- httr::content(response, "text",encoding="UTF-8")

  # Conte\u00fado j\u00e1 verificado

  res <- jsonlite::parse_json(json_data)


  res <-
    tibble::tibble(json=res)|>
    tidyr::unnest_wider("json")|>
    tidyr::unnest_longer("resultados")|>
    tidyr::unnest_wider("resultados")|>
    tidyr::unnest_longer("classificacoes")|>
    tidyr::unnest_wider("classificacoes",names_sep="_")|>
    tidyr::unnest_longer("classificacoes_categoria")|>
    tidyr::unnest_longer("series")|>
    tidyr::unnest_wider("series")|>
    tidyr::unnest_wider("localidade",names_sep="_")|>
    tidyr::unnest_wider("localidade_nivel",names_sep="_")|>tidyr::unnest_longer("serie")|>
    dplyr::rename(valor="serie",periodo="serie_id")|>
    suppressWarnings(dplyr::mutate(dplyr::across(c("valor","periodo","id","localidade_id",
                    "classificacoes_id","classificacoes_categoria_id"),as.numeric)))|>
    tidyr::pivot_wider(
      names_from = c("classificacoes_id","classificacoes_categoria_id","classificacoes_categoria"),
      names_sep = "-",names_prefix = "c",values_from="valor")|>
    dplyr::select(-c("classificacoes_nome"))

  return(res)

}
