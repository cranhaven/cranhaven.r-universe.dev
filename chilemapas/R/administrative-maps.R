#' Mapa de Chile a nivel provincial
#' @description Este mapa es calculado a partir
#' del mapa comunal para no recargar el volumen de datos del paquete.
#' @param mapa mapa a agregar, por defecto es todo el mapa nacional
#' @import sf
#' @importFrom sf st_as_sf st_union
#' @importFrom dplyr as_tibble inner_join select distinct group_by summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @return Un objeto de clase sf y data.frame.
#' @examples
#' generar_provincias()
#' @export
generar_provincias <- function(mapa = chilemapas::mapa_comunas) {
  st_as_sf(mapa) %>%
    group_by(!!sym("codigo_provincia")) %>%
    summarise(geometry = st_union(!!sym("geometry")), .groups = "drop") %>%
    as_tibble() %>%
    inner_join(
      chilemapas::codigos_territoriales %>%
        select(!!sym("codigo_provincia"), !!sym("codigo_region")) %>%
        distinct(),
      by = "codigo_provincia"
    ) %>%
    select(!!sym("codigo_provincia"), !!sym("codigo_region"), !!sym("geometry"))
}

#' Mapa de Chile a nivel regional
#' @description Este mapa es calculado a partir
#' del mapa comunal para no recargar el volumen de datos del paquete.
#' @param mapa mapa a agregar, por defecto es todo el mapa nacional
#' @importFrom sf st_as_sf st_union
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @return Un objeto de clase sf y data.frame.
#' @examples
#' generar_regiones()
#' @export
generar_regiones <- function(mapa = chilemapas::mapa_comunas) {
  st_as_sf(mapa) %>%
    group_by(!!sym("codigo_region")) %>%
    summarise(geometry = st_union(!!sym("geometry")), .groups = "drop")
}

#' Mapa de Chile a nivel de servicios de salud
#' @description Este mapa es calculado a partir
#' del mapa comunal para no recargar el volumen de datos del paquete.
#' @param mapa mapa a agregar, por defecto es todo el mapa nacional
#' @importFrom dplyr as_tibble inner_join
#' @importFrom sf st_as_sf st_union
#' @importFrom dplyr group_by summarise select distinct
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @return Un objeto de clase sf y data.frame.
#' @examples
#' generar_servicios_salud()
#' @export
generar_servicios_salud <- function(mapa = chilemapas::mapa_comunas) {
  mapa %>%
    merge(
      chilemapas::divisiones_salud %>%
        select(!!sym("codigo_servicio_salud"), !!sym("codigo_comuna")),
      by = "codigo_comuna"
    ) %>%
    st_as_sf() %>%
    group_by(!!sym("codigo_servicio_salud")) %>%
    summarise(geometry = st_union(!!sym("geometry")), .groups = "drop") %>%
    as_tibble() %>%
    inner_join(
      chilemapas::divisiones_salud %>%
        select(!!sym("codigo_servicio_salud"), !!sym("codigo_comuna")) %>%
        distinct() %>%
        inner_join(
          chilemapas::codigos_territoriales %>%
            select(!!sym("codigo_comuna"), !!sym("codigo_region"))
        ) %>%
        select(!!sym("codigo_servicio_salud"), !!sym("codigo_region")) %>%
        distinct(),
      by = "codigo_servicio_salud"
    ) %>%
    select(!!sym("codigo_servicio_salud"), !!sym("codigo_region"), !!sym("geometry"))
}
