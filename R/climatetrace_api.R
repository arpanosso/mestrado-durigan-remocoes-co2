
# URL base
base_url <- "https://api.climatetrace.org"


#' Lista os países disponíveis na API
#'
#' Esta função realiza uma requisição GET para a API definida pela variável \code{base_url}
#' e retorna uma lista com os países disponíveis conforme o endpoint \code{/v6/definitions/countries}.
#'
#' @return Um objeto (geralmente uma lista ou data frame) contendo os países retornados pela API.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' base_url <- "https://api.climatetrace.org"  # Defina a base_url antes de usar
#' paises <- get_countries()
#' print(paises)
#' }
get_countries <- function() {
  url <- paste0(base_url, "/v6/definitions/countries")
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Lista os setores disponíveis na API
#'
#' Consulta o endpoint `/v6/definitions/sectors` e retorna os setores disponíveis.
#'
#' @return Um objeto contendo os setores retornados pela API.
#' @export
get_sectors <- function() {
  url <- paste0(base_url, "/v6/definitions/sectors")
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Lista os subsetores disponíveis na API
#'
#' Consulta o endpoint `/v6/definitions/subsectors` e retorna os subsetores disponíveis.
#'
#' @return Um objeto contendo os subsetores retornados pela API.
#' @export
get_subsectors <- function() {
  url <- paste0(base_url, "/v6/definitions/subsectors")
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Lista os continentes disponíveis na API
#'
#' Consulta o endpoint `/v6/definitions/continents` e retorna os continentes disponíveis.
#'
#' @return Um objeto contendo os continentes retornados pela API.
#' @export
get_continents <- function() {
  url <- paste0(base_url, "/v6/definitions/continents")
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Lista os grupos disponíveis na API
#'
#' Consulta o endpoint `/v6/definitions/groups` e retorna os grupos disponíveis.
#'
#' @return Um objeto contendo os grupos retornados pela API.
#' @export
get_groups <- function() {
  url <- paste0(base_url, "/v6/definitions/groups")
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Lista os gases disponíveis na API
#'
#' Consulta o endpoint `/v6/definitions/gases` e retorna os gases rastreados pela plataforma.
#'
#' @return Um objeto contendo os gases retornados pela API.
#' @export
get_gases <- function() {
  url <- paste0(base_url, "/v6/definitions/gases")
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Busca fontes de emissão (assets) na API do Climate TRACE
#'
#' Esta função consulta o endpoint \code{/v6/assets} da API do Climate TRACE para buscar
#' fontes de emissão com base em filtros como países, setores, subsetores e ano.
#'
#' @param countries Vetor de códigos de países (ISO Alpha-3) a serem filtrados. Opcional.
#' @param sectors Vetor de setores a serem filtrados. Opcional.
#' @param subsectors Vetor de subsetores a serem filtrados. Opcional.
#' @param year Ano de referência dos dados (padrão: 2022).
#' @param limit Número máximo de registros a retornar (padrão: 50).
#' @param offset Deslocamento inicial dos resultados (para paginação; padrão: 0).
#'
#' @return Um objeto (geralmente uma lista ou data frame) contendo os ativos de emissão conforme os filtros definidos.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' base_url <- "https://api.climatetrace.org"
#' ativos <- search_assets(countries = c("BRA"), year = 2022, limit = 10)
#' print(ativos)
#' }
search_assets <- function(countries = NULL, sectors = NULL, subsectors = NULL,
                          year = 2022, limit = 50, offset = 0) {
  url <- paste0(base_url, "/v6/assets")

  # Preparando parâmetros de consulta
  query_params <- list(
    year = year,
    limit = limit,
    offset = offset
  )

  # Adicionando parâmetros opcionais
  if (!is.null(countries)) {
    query_params$countries <- paste(countries, collapse = ",")
  }

  if (!is.null(sectors)) {
    query_params$sectors <- paste(sectors, collapse = ",")
  }

  if (!is.null(subsectors)) {
    query_params$subsectors <- paste(subsectors, collapse = ",")
  }

  response <- httr::GET(url, query = query_params)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Obtém detalhes de uma fonte de emissão específica
#'
#' Esta função consulta o endpoint \code{/v6/assets/{source_id}} da API do Climate TRACE
#' e retorna as informações detalhadas sobre uma fonte de emissão identificada por \code{source_id}.
#'
#' @param source_id Identificador único da fonte de emissão (asset) a ser consultada.
#'
#' @return Um objeto contendo os detalhes da fonte de emissão especificada.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' base_url <- "https://api.climatetrace.org"
#' detalhes <- get_asset_details("asset_id_exemplo")
#' print(detalhes)
#' }
get_asset_details <- function(source_id) {
  url <- paste0(base_url, "/v6/assets/", source_id)
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Filtra e sumariza emissões de fontes de emissão (assets)
#'
#' Consulta o endpoint \code{/v6/assets/emissions} da API do Climate TRACE, retornando
#' as emissões agregadas com base em filtros como países, setores, subsetores, anos, gás e unidade administrativa.
#'
#' @param countries Vetor de códigos de países (ISO Alpha-3). Opcional.
#' @param sectors Vetor de setores. Opcional.
#' @param subsectors Vetor de subsetores. Opcional.
#' @param years Vetor de anos a serem considerados. Opcional.
#' @param gas Gás específico (por exemplo, "CO2", "CH4", "N2O"). Opcional.
#' @param admin_id Código da unidade administrativa (como um município ou estado). Opcional.
#'
#' @return Um objeto contendo as emissões filtradas e agregadas conforme os critérios definidos.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' base_url <- "https://api.climatetrace.org"
#' emissoes <- get_asset_emissions(
#'   countries = c("BRA"),
#'   sectors = c("Agriculture"),
#'   years = 2022,
#'   gas = "CO2"
#' )
#' print(emissoes)
#' }
get_asset_emissions <- function(countries = NULL, sectors = NULL, subsectors = NULL,
                                years = NULL, gas = NULL, admin_id = NULL) {
  url <- paste0(base_url, "/v6/assets/emissions")

  # Preparando parâmetros de consulta
  query_params <- list()

  # Adicionando parâmetros opcionais
  if (!is.null(countries)) {
    query_params$countries <- paste(countries, collapse = ",")
  }

  if (!is.null(sectors)) {
    query_params$sectors <- paste(sectors, collapse = ",")
  }

  if (!is.null(subsectors)) {
    query_params$subsectors <- paste(subsectors, collapse = ",")
  }

  if (!is.null(years)) {
    query_params$years <- paste(years, collapse = ",")
  }

  if (!is.null(gas)) {
    query_params$gas <- gas
  }

  if (!is.null(admin_id)) {
    query_params$adminId <- admin_id
  }

  response <- httr::GET(url, query = query_params)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Obtém as emissões de gases de efeito estufa por país
#'
#' Consulta o endpoint \code{/v6/country/emissions} da API do Climate TRACE, retornando
#' as emissões anuais por país com base em filtros opcionais como setor e subsetores.
#'
#' @param countries Vetor de códigos de países (ISO Alpha-3). Opcional.
#' @param sector Setor principal de atividade (ex: "Agriculture"). Opcional.
#' @param subsectors Vetor de subsetores de atividade. Opcional.
#' @param since Ano inicial do período (padrão: 2010).
#' @param to Ano final do período (padrão: 2022).
#'
#' @return Um objeto com as emissões por país, agrupadas por ano.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' base_url <- "https://api.climatetrace.org"
#' emissoes_pais <- get_country_emissions(
#'   countries = c("BRA", "ARG"),
#'   sector = "Agriculture",
#'   since = 2015,
#'   to = 2022
#' )
#' print(emissoes_pais)
#' }
get_country_emissions <- function(countries = NULL, sector = NULL, subsectors = NULL,
                                  since = 2010, to = 2022) {
  url <- paste0(base_url, "/v6/country/emissions")

  # Preparando parâmetros de consulta
  query_params <- list(
    since = since,
    to = to
  )

  # Adicionando parâmetros opcionais
  if (!is.null(countries)) {
    query_params$countries <- paste(countries, collapse = ",")
  }

  if (!is.null(sector)) {
    query_params$sector <- paste(sector, collapse = ",")
  }

  if (!is.null(subsectors)) {
    query_params$subsectors <- paste(subsectors, collapse = ",")
  }

  response <- httr::GET(url, query = query_params)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Busca áreas administrativas disponíveis na base do Climate TRACE
#'
#' Consulta o endpoint \code{/v6/admins/search} para retornar informações sobre
#' áreas administrativas (como estados ou municípios), podendo filtrar por nome e nível.
#'
#' @param name Nome (ou parte do nome) da área administrativa. Opcional.
#' @param level Nível da administração (por exemplo: 1 para estados, 2 para municípios). Opcional.
#' @param limit Número máximo de registros retornados (padrão: 50).
#' @param offset Número de registros a pular (padrão: 0).
#'
#' @return Um data frame com as áreas administrativas encontradas.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' search_admins(name = "São Paulo", level = 2)
#' }
search_admins <- function(name = NULL, level = NULL, limit = 50, offset = 0) {
  url <- paste0(base_url, "/v6/admins/search")

  query_params <- list(
    limit = limit,
    offset = offset
  )

  if (!is.null(name)) {
    query_params$name <- name
  }

  if (!is.null(level)) {
    query_params$level <- level
  }

  response <- httr::GET(url, query = query_params)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Obtém dados GeoJSON de uma área administrativa
#'
#' Consulta o endpoint \code{/v6/admins/{admin_id}/geojson} e retorna os dados
#' geoespaciais no formato GeoJSON para uma área administrativa específica.
#'
#' @param admin_id Identificador único da área administrativa.
#'
#' @return Um objeto em formato GeoJSON (lista em R).
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' geo <- get_admin_geojson("BRA.27.245")
#' print(geo)
#' }
get_admin_geojson <- function(admin_id) {
  url <- paste0(base_url, "/v6/admins/", admin_id, "/geojson")
  response <- httr::GET(url)
  return(jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8")))
}

#' Função auxiliar para explorar a estrutura de uma resposta da API
#'
#' Realiza uma requisição GET para uma URL da API do Climate TRACE, imprime
#' a estrutura da resposta (com \code{str}) e retorna o conteúdo.
#'
#' @param url URL completa do endpoint a ser consultado.
#'
#' @return Conteúdo da resposta como lista ou data frame.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' explore_api("https://api.climatetrace.org/v6/definitions/sectors")
#' }
explore_api <- function(url) {
  response <- httr::GET(url)
  content <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  str(content)
  return(content)
}
