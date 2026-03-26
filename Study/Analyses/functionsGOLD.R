addCountry <- function(cohort){
  name = tableName(cohort)
  wales_id <- cdm$location|> dplyr::collect() |> dplyr::filter(.data$country_source_value == "Wales") |> dplyr::pull(.data$location_id)
  scotland_id <- cdm$location|> dplyr::collect() |> dplyr::filter(.data$country_source_value == "Scotland") |> dplyr::pull(.data$location_id)
  ni_id <- cdm$location|> dplyr::collect() |> dplyr::filter(.data$country_source_value == "Northern Ireland") |> dplyr::pull(.data$location_id)
  england_id <- cdm$location|> dplyr::collect() |> dplyr::filter(!.data$country_source_value %in% c("Wales", "Scotland", "Northern Ireland")) |> dplyr::pull(.data$location_id)
  
  wales_care_sites <- cdm$care_site |> dplyr::collect() |> dplyr::filter(location_id %in% wales_id) |> pull(care_site_id)
  scotland_care_sites <- cdm$care_site |> dplyr::collect() |> dplyr::filter(location_id %in% scotland_id) |> pull(care_site_id)
  ni_care_sites <- cdm$care_site |> dplyr::collect() |> dplyr::filter(location_id %in% ni_id) |> pull(care_site_id)
  england_care_sites <- cdm$care_site |> dplyr::collect() |> dplyr::filter(location_id %in% england_id) |> pull(care_site_id)
  
  cohort |> dplyr::left_join(cdm$person |> 
                               dplyr::select("person_id", "care_site_id"),
                             by = c("subject_id" = "person_id")) |>
    dplyr::mutate(country = dplyr::case_when(
      care_site_id %in% wales_care_sites ~ "Wales",
      care_site_id %in% scotland_care_sites ~ "Scotland",
      care_site_id %in% ni_care_sites ~ "Northern Ireland",
      care_site_id %in% england_care_sites ~ "England",
      TRUE ~ "Unknown"
    )) |>
    dplyr::select(-care_site_id) |>
    compute(name = name, temporary = FALSE)
  
}
