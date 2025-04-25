#### Download full list of sources #
ctrace_download_sources <- function(countries = NULL,
                                    sectors = NULL,
                                    subsectors = NULL,
                                    file_path = NULL,
                                    continents = NULL,
                                    groups = NULL,
                                    ownership = FALSE) {

  # check args
  args <- list(
    continents = continents,
    countries = countries,
    sectors = sectors,
    subsectors = subsectors
  ) |>
    purrr::compact() |>
    purrr::map(\(x) paste(x, collapse = ","))


  request <- httr2::request("https://api.climatetrace.org/v6") |>
    httr2::req_url_path_append("assets") |>
    httr2::req_url_query(!!!args, limit = 0)

  resp <- request |> httr2::req_perform() |>
    httr2::resp_body_json() |>
    _[["assets"]]


  # Clean data

  gas_list <- c("n2o", "co2", "ch4", "co2e_20yr", "co2e_100yr")

   df <- resp |> tibble::tibble()  |>
     tidyr::unnest_wider("resp") #|>

    # tidyr::unnest_longer(Emissions) |>
    # tidyr::unnest_longer(Emissions, simplify = TRUE) |>
    # tidyr::unnest_longer(Emissions)  |>
  #
  #   dplyr::mutate(gas = purrr::map(Emissions, \(x) names(x)[names(x) %in% gas_list])) |>
  #
  #   dplyr::mutate(emission = suppressWarnings(
  #     as.numeric(as.character(
  #       purrr::map(Emissions,
  #                  \(x) purrr::pluck(x,
  #                                    names(x)[names(x) %in% gas_list]))))))|>
  #
  #   tidyr::hoist(Emissions, "Activity", "ActivityUnits", "Capacity",
  #                "CapacityUnits", "CapacityFactor", "EmissionsFactor",
  #                "EmissionsFactorUnits") |>
  #
  #   dplyr::mutate(CapacityUnits = dplyr::na_if(CapacityUnits, "N/A")) |>
  #
  #   tidyr::hoist(Centroid, lat = list("Geometry", 2), lon = list("Geometry", 1)) |>    ##### TODO drop dates
  #
  #   (\(x) {if (ownership) {tidyr::unnest_wider(x, Owners, simplify = TRUE)} else x})() |>
  #
  #   dplyr::select(-Owners, -Centroid, -Thumbnail, -Confidence, -NativeId, -ReportingEntity, -Emissions) |>
  #
  #   dplyr::rename("Year" = Emissions_id,
  #                 "Emission" = emission,
  #                 "Gas" = gas) |>
  #
  #   dplyr::relocate("Emission", "Gas", "EmissionsFactor", "EmissionsFactorUnits", .after = ActivityUnits) |>
  #
  #   dplyr::relocate("Year",, .after = "AssetType")

    # TODO Renaming of columns according to already used pattern



  if (!is.null(file_path)) {
    write.table(df, file = file_path , row.names = FALSE)
    messsage("File downloaded successfully")
  } else return(df)
}

# TODO handle especial cases like forestry and land use, make them explicit, maybe return table with emissions by sector

######################################################################################################

##### Download total emisisons by sector

ctrace_download_summary <- function(years = NULL, # TODO unused?
                                    gas = NULL,   # TODO accept list
                                    sectors = NULL,
                                    subsectors = NULL,
                                    countries = NULL,
                                    continents = NULL,
                                    groups = NULL,
                                    file_path = NULL){

  # check args
  args <- list(
    years = years,
    gas = gas,  # TODO respond gas list by multiple requests and appending
    sectors = sectors,
    subsectors = subsectors,
    countries = countries,
    continents = continents,
    groups = groups
  ) |>
    purrr::compact() |>
    purrr::map(\(x) paste(x, collapse = ","))

  # TODO add total emissions for country and world

  # TODO aggregate gas information if no gas(es) are supplied

  request <- httr2::request("https://api.climatetrace.org/v4") |>
    httr2::req_url_path_append("assets/emissions") |>
    httr2::req_url_query(!!!args)

  df <- request |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    purrr::map(c) |>
    dplyr::bind_rows() |>
    tibble::as_tibble()

  return(df)

}
