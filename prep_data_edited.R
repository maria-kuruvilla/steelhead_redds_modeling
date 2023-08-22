#need new query data function and need to send new file paths to prep_wen_sthd_data.

# load needed libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(janitor)
library(magrittr)
library(msm)
library(here)
library(DescTools)
library(PITcleanr)
library(sroem)

#####
#new functions

query_redd_data <- function(
    redd_file_path = "data",
    redd_file_name = "STHD_Wenatchee_Redd_Surveys.xlsx",
    experience_path = "data",
    experience_file_name = "STHD_Surveyor_Experience.xlsx",
    query_year = yrs) {
  
  data_file = paste(redd_file_path,
                    redd_file_name,
                    sep = "/")
  
  if(!file.exists(data_file)) {
    stop("File not found.")
  }
  
  data_list <- readxl::excel_sheets(data_file) |>
    as.list() |>
    rlang::set_names() |>
    purrr::map(.f = purrr::quietly(function(x) {
      readxl::read_excel(data_file,
                         sheet = x) |>
        janitor::clean_names()
    })) |>
    purrr::map("result")
  
  
  redd_surv_df <- data_list$`Redd Surveys` |>
    dplyr::mutate(
      across(
        c(survey_type,
          river),
        stringr::str_to_title
      )
    )
  
  # to-date, only grab experience covariates from Wenatchee data
  if(stringr::str_detect(redd_file_name, "Wenatchee")) {
    
    # get experience data
    exp_df <- suppressMessages(readxl::read_excel(paste(experience_path,
                                                        experience_file_name,
                                                        sep = "/"),
                                                  sheet = "Experience",
                                                  skip = 1)) |>
      dplyr::rename(basin = `...1`,
                    surveyor_initials = `...2`) |>
      tidyr::pivot_longer(-c(basin:surveyor_initials),
                          names_to = "spawn_year",
                          values_to = "experience") |>
      dplyr::mutate(
        dplyr::across(
          spawn_year,
          as.numeric
        )
      )
    
    redd_surv_df <- redd_surv_df |>
      dplyr::mutate(
        dplyr::across(
          c(surveyor1,
            surveyor2),
          ~ stringr::str_remove(.,
                                "\\ \\([:alpha:]+\\)"))) |>
      dplyr::left_join(exp_df |>
                         dplyr::select(spawn_year,
                                       surveyor1 = surveyor_initials,
                                       exp1 = experience),
                       by = c("spawn_year", "surveyor1")) |>
      dplyr::left_join(exp_df |>
                         dplyr::select(spawn_year,
                                       surveyor2 = surveyor_initials,
                                       exp2 = experience),
                       by = c("spawn_year", "surveyor2")) |>
      dplyr::rowwise() |>
      dplyr::mutate(exp_sp_total = mean(c(exp1, exp2), na.rm = T)) |>
      dplyr::ungroup()
  }
  
  redd_df <- redd_surv_df |>
    dplyr::left_join(data_list$`Reach Length` |>
                       dplyr::group_by(river,
                                       reach,
                                       type, index) |>
                       dplyr::summarize(
                         dplyr::across(length_km,
                                       ~ sum(.))),
                     by = c("river", "reach", "index")) |>
    dplyr::left_join(data_list$`Thalweg CV` |>
                       dplyr::select(river,
                                     reach,
                                     mean_thalweg_cv),
                     by = c("river", "reach")) |>
    dplyr::left_join(data_list$`Discharge Gages` |>
                       dplyr::select(reach,
                                     usgs_site_code = site_code),
                     by = "reach") |>
    # left_join(data_list$Discharge,
    #           by = c("spawn_year", "river", "reach", "index", "survey_type", "survey_date")) |>
    dplyr::mutate(
      dplyr::across(
        c(reach,
          river),
        as.factor),
      dplyr::across(
        reach,
        forcats::fct_relevel,
        "W10",
        "C1", "N1", "P1",
        after = Inf),
      dplyr::across(
        reach,
        forcats::fct_relevel,
        "MH1", "T1", "WN1",
        after = Inf)) |>
    dplyr::filter(spawn_year %in% query_year) |>
    # calculate redd density and log of experience
    dplyr::mutate(naive_density_km = visible_redds / length_km,
                  exp_sp_total_log = log(exp_sp_total))
  
  if(nrow(redd_df) == 0) {
    message(paste("No redd data found for",
                  paste(query_year, collapse = ", "),
                  ".\n"))
    return(NULL)
  }
  
  message("\t Querying USGS for discharge data\n")
  
  # query USGS for mean daily discharge data
  discharge_df <- redd_df |>
    dplyr::filter(!is.na(usgs_site_code)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dataRetrieval::readNWISdv(usgs_site_code,
                                parameterCd = "00060", # discharge
                                startDate = as.character(lubridate::ymd(survey_date)),
                                endDate = as.character(lubridate::ymd(survey_date)),
                                statCd = "00003" # mean
      )) |>
    dplyr::ungroup() |>
    dplyr::rename(mean_discharge = X_00060_00003) |>
    dplyr::select(-c(agency_cd:Date, X_00060_00003_cd))
  
  # adjust discharge for W10: Plain - Chiwawa discharge
  if("W10" %in% unique(redd_df$reach)) {
    plain_code <- redd_df |>
      dplyr::filter(reach == "W9") |>
      dplyr::pull(usgs_site_code) |>
      unique()
    
    w10_discharge <- discharge_df |>
      dplyr::filter(reach == "W10") |>
      dplyr::rowwise() |>
      dplyr::mutate(
        dataRetrieval::readNWISdv(plain_code,
                                  parameterCd = "00060", # discharge
                                  startDate = as.character(lubridate::ymd(survey_date)),
                                  endDate = as.character(lubridate::ymd(survey_date)),
                                  statCd = "00003" # mean
        )) |>
      dplyr::ungroup() |>
      dplyr::rename(plain_discharge = X_00060_00003) |>
      dplyr::select(-c(agency_cd:Date, X_00060_00003_cd)) |>
      dplyr::mutate(mean_discharge = plain_discharge - mean_discharge) |>
      dplyr::select(-plain_discharge)
    
    # put it all back together
    discharge_df |>
      dplyr::filter(reach != "W10") |>
      dplyr::bind_rows(w10_discharge) -> discharge_df
    
  }
  
  redd_data <- discharge_df |>
    dplyr::bind_rows(redd_df |>
                       dplyr::filter(is.na(usgs_site_code))) |>
    dplyr::arrange(spawn_year,
                   river,
                   reach,
                   index,
                   survey_date)
  
  # identical(nrow(redd_df),
  #           nrow(redd_data))
  
  
  return(redd_data)
  
}

prep_wen_sthd_data <- function(
    redd_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Redd Data",
    redd_file_name = "Wenatchee_Redd_Surveys.xlsx",
    experience_path = redd_file_path,
    experience_file_name = redd_file_name,
    dabom_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/estimates",
    dabom_file_name = "UC_STHD_Model_Output.xlsx",
    brood_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Bio Data/Sex and Origin PRD-Brood Comparison Data",
    brood_file_name = "STHD UC Brood Collections_2011 to current.xlsx",
    removal_file_path = "T:/DFW-Team FP Upper Columbia Escapement - General/UC_Sthd/inputs/Fish Removals",
    removal_file_name = "Master_STHD_Removals_2.18.23.MH.xlsx",
    n_observers = "two",
    query_year = lubridate::year(lubridate::today()) - 1,
    save_rda = F,
    save_by_year = T,
    save_file_path = here::here("analysis/data/derived_data"),
    save_file_name = NULL
) {
  
  message("\t Gathering redd data.\n")
  
  # load data for selected years
  redd_df_all <- query_redd_data(redd_file_path,
                                 redd_file_name,
                                 experience_path,
                                 experience_file_name,
                                 query_year = query_year)
  
  if(!is.null(redd_df_all)) {
    # divide reaches into various location categories
    redd_df_all <- redd_df_all |>
      dplyr::mutate(location = dplyr::if_else(reach %in% paste0("W", 8:10),
                                              "Above Tumwater",
                                              dplyr::if_else(reach %in% paste0("W", 1:7),
                                                             "Below Tumwater",
                                                             "Tributaries")))
    
    # predict net error
    redd_df_all <- redd_df_all |>
      sroem::predict_neterr(species = "Steelhead",
                            num_obs = n_observers)
  }
  
  #-----------------------------------------------------------------
  # load data on error calls for sex at Priest Rapids when fish were tagged
  
  message("\t Pulling PIT tag data.\n\n")
  
  # get info on tags detected somewhere in the Wenatchee
  wen_tags_all <- readxl::read_excel(paste(dabom_file_path,
                                           dabom_file_name,
                                           sep = "/"),
                                     sheet = "Tag Summary") |>
    janitor::clean_names() |>
    dplyr::filter(str_detect(path, "LWE"),
                  spawn_year %in% query_year) |>
    dplyr::mutate(location = dplyr::if_else(spawn_node %in% c('TUM', 'UWE'),
                                            'Above Tumwater',
                                            dplyr::if_else(str_detect(spawn_node, "^LWE"),
                                                           "Below Tumwater",
                                                           dplyr::if_else(str_detect(path, "CHL"),
                                                                          "Chiwawa",
                                                                          dplyr::if_else(str_detect(path, "NAL"),
                                                                                         "Nason",
                                                                                         dplyr::if_else(str_detect(path, "PES"),
                                                                                                        "Peshastin",
                                                                                                        "Other Tributaries"))))),
                  dplyr::across(location,
                                factor,
                                levels = c("Below Tumwater",
                                           'Above Tumwater',
                                           "Peshastin",
                                           "Nason",
                                           "Chiwawa",
                                           'Other Tributaries'))) |>
    dplyr::select(spawn_year,
                  tag_code,
                  location,
                  origin,
                  sex)
  
  #-------------------------------------------------------
  # generate fish / redd and pHOS for different areas
  fpr_all = wen_tags_all |>
    dplyr::group_by(spawn_year,
                    location) |>
    dplyr::summarize(n_male = n_distinct(tag_code[sex == "M"]),
                     n_female = n_distinct(tag_code[sex == "F"]),
                     n_sexed = n_male + n_female,
                     n_wild = n_distinct(tag_code[origin == "W"]),
                     n_hatch = n_distinct(tag_code[origin == "H"]),
                     n_origin = n_wild + n_hatch,
                     .groups = "drop") |>
    dplyr::mutate(prop_m = n_male / n_sexed,
                  prop_se = sqrt((prop_m * (1 - prop_m)) / (n_sexed)),
                  fpr = (prop_m) / (1 - prop_m) + 1) |>
    dplyr::rowwise() |>
    dplyr::mutate(fpr_se = msm::deltamethod(~ x1 / (1 - x1) + 1,
                                            mean = prop_m,
                                            cov = prop_se^2)) |>
    dplyr::ungroup() |>
    dplyr::mutate(phos = n_hatch / n_origin,
                  phos_se = sqrt((phos * (1 - phos)) / (n_origin)))
  
  message("\t Adjusting fish/redd.\n")
  
  # adjust fish / redd for errors in Priest sex calls
  # the excel file contains rounded numbers, so re-calculate
  # various statistics for use in analyses
  # estimate error rate for each sex
  sex_err_rate <- readxl::read_excel(paste(dabom_file_path,
                                           dabom_file_name,
                                           sep = "/"),
                                     sheet = "Tag Summary") |>
    janitor::clean_names() |>
    select(spawn_year,
           tag_code,
           sex_field = sex) |>
    inner_join(read_excel(paste(brood_file_path,
                                brood_file_name,
                                sep = "/"),
                          sheet = "Brood Collected_PIT Tagged Only") |>
                 clean_names() |>
                 rename(tag_code = recaptured_pit) |>
                 select(spawn_year,
                        tag_code,
                        sex_final) |>
                 distinct(),
               by = c("spawn_year",
                      "tag_code")) |>
    filter(!is.na(sex_final),
           !is.na(sex_field)) |>
    mutate(agree = if_else(sex_field == sex_final,
                           T, F)) |>
    group_by(spawn_year,
             sex = sex_field) |>
    summarize(n_tags = n_distinct(tag_code),
              n_true = sum(agree),
              n_false = sum(!agree),
              .groups = "drop") |>
    mutate(binom_ci = map2(n_false,
                           n_tags,
                           .f = function(x, y) {
                             DescTools::BinomCI(x, y) |>
                               as_tibble()
                           })) |>
    unnest(binom_ci) |>
    clean_names() |>
    rename(perc_false = est,
           lowerci = lwr_ci,
           upperci = upr_ci) |>
    mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags)) |>
    relocate(perc_se,
             .after = "perc_false")
  
  #
  #
  # sex_err_rate <- readxl::read_excel(paste(dabom_file_path,
  #                                          dabom_file_name,
  #                                          sep = "/"),
  #                                    sheet = "Sex Error Rates") |>
  #   janitor::clean_names() |>
  #   dplyr::select(spawn_year:n_false) |>
  #   dplyr::filter(spawn_year %in% query_year) |>
  #   dplyr::rowwise() |>
  #   dplyr::mutate(binom_ci = map2(n_false,
  #                                 n_tags,
  #                                 .f = function(x, y) {
  #                                   DescTools::BinomCI(x, y) |>
  #                                     as_tibble()
  #                                 })) |>
  #   tidyr::unnest(binom_ci) |>
  #   janitor::clean_names() |>
  #   dplyr::rename(perc_false = est) |>
  #   dplyr::mutate(perc_se = sqrt((perc_false * (1 - perc_false)) / n_tags))
  
  adj_fpr <- fpr_all |>
    dplyr::select(spawn_year,
                  location,
                  n_male,
                  n_female) |>
    tidyr::pivot_longer(cols = c(n_male,
                                 n_female),
                        names_to = "sex",
                        values_to = "n_fish") |>
    dplyr::mutate(
      dplyr::across(sex,
                    str_remove,
                    "^n_"),
      dplyr::across(sex,
                    str_to_title)) |>
    dplyr::mutate(
      dplyr::across(sex,
                    recode,
                    "Male" = "M",
                    "Female" = "F")) |>
    dplyr::left_join(sex_err_rate |>
                       dplyr::select(spawn_year,
                                     sex,
                                     dplyr::starts_with("perc_")),
                     by = c("spawn_year", "sex")) |>
    tidyr::pivot_wider(names_from = sex,
                       values_from = c(n_fish,
                                       perc_false,
                                       perc_se)) |>
    dplyr::mutate(true_male = n_fish_M - (n_fish_M * perc_false_M) + (n_fish_F * perc_false_F),
                  true_female = n_fish_F - (n_fish_F * perc_false_F) + (n_fish_M * perc_false_M),
                  dplyr::across(starts_with("true"),
                                janitor::round_half_up)) |>
    dplyr::rowwise() |>
    dplyr::mutate(true_m_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                               mean = c(n_fish_M,
                                                        perc_false_M,
                                                        n_fish_F,
                                                        perc_false_F),
                                               cov = diag(c(0,
                                                            perc_se_M,
                                                            0,
                                                            perc_se_F)^2)),
                  true_f_se = msm::deltamethod(~ x1 - (x1 * x2) + (x3 * x4),
                                               mean = c(n_fish_F,
                                                        perc_false_F,
                                                        n_fish_M,
                                                        perc_false_M),
                                               cov = diag(c(0,
                                                            perc_se_F,
                                                            0,
                                                            perc_se_M)^2))) |>
    dplyr::mutate(n_sexed = true_male + true_female,
                  prop_m = true_male / (true_male + true_female),
                  prop_se = msm::deltamethod(~ x1 / (x1 + x2),
                                             mean = c(true_male,
                                                      true_female),
                                             cov = diag(c(true_m_se,
                                                          true_f_se)^2)),
                  fpr = (prop_m) / (1 - prop_m) + 1,
                  fpr_se = msm::deltamethod(~ x1 / (1 - x1) + 1,
                                            mean = prop_m,
                                            cov = prop_se^2)) |>
    dplyr::ungroup() |>
    dplyr::rename(n_male = true_male,
                  n_female = true_female) |>
    dplyr::left_join(fpr_all |>
                       dplyr::select(spawn_year,
                                     location,
                                     n_wild,
                                     n_hatch,
                                     n_origin,
                                     starts_with("phos")),
                     by = c("spawn_year", "location")) |>
    dplyr::select(dplyr::any_of(names(fpr_all)))
  
  # # look at changes to fish/redd
  # fpr_all |>
  #   select(spawn_year,
  #          location,
  #          old_fpr = fpr) |>
  #   left_join(adj_fpr |>
  #               select(spawn_year,
  #                      location,
  #                      adj_fpr = fpr))
  
  # if any fpr values are Inf, use the older ones
  if(sum(adj_fpr$fpr == Inf) > 0) {
    adj_fpr <- adj_fpr |>
      dplyr::left_join(fpr_all |>
                         dplyr::select(location,
                                       old_fpr = fpr,
                                       old_se = fpr_se)) |>
      dplyr::mutate(fpr = dplyr::if_else(is.na(fpr) | fpr == Inf,
                                         old_fpr,
                                         fpr),
                    fpr_se = dplyr::if_else(is.na(fpr_se) | fpr_se == Inf,
                                            old_se,
                                            fpr_se)) |>
      dplyr::select(-dplyr::starts_with("old"))
  }
  
  fpr_all <- adj_fpr
  
  rm(adj_fpr)
  
  #-----------------------------------------------------------------
  # read in data about known removals of fish prior to spawning
  if(file.exists(paste(removal_file_path,
                       removal_file_name,
                       sep = "/"))) {
    if(str_detect(removal_file_name, "csv$")) {
      removal_df <- readr::read_csv(paste(removal_file_path,
                                          removal_file_name,
                                          sep = "/")) |>
        janitor::clean_names() |>
        dplyr::filter(subbasin == "Wenatchee",
                      spawn_year %in% query_year)
    }
    if(str_detect(removal_file_name, "xls$") |
       str_detect(removal_file_name, "xlsx$")) {
      
      removal_df <- readxl::read_excel(paste(removal_file_path,
                                             removal_file_name,
                                             sep = "/"),
                                       skip = 3,
                                       col_names = c("run cycle",
                                                     "spawn year",
                                                     "population",
                                                     "removal location",
                                                     "agency",
                                                     "adult trapping surplus H",
                                                     "brood collections H",
                                                     "harvest H",
                                                     "adult trapping surplus W",
                                                     "brood collections W",
                                                     "harvest W",
                                                     "adult trapping surplus T",
                                                     "brood collections T",
                                                     "harvest T")) |>
        janitor::clean_names() |>
        dplyr::mutate(
          across(
            c(ends_with("_h"),
              ends_with("_w"),
              ends_with("_t")),
            as.numeric
          )) |>
        dplyr::filter(!is.na(spawn_year)) |>
        dplyr::select(spawn_year:harvest_w) |>
        tidyr::pivot_longer(cols = c(ends_with("_h"),
                                     ends_with("_w")),
                            names_to = "source",
                            values_to = "removed") |>
        dplyr::mutate(origin = str_sub(source, -1)) |>
        dplyr::relocate(origin,
                        .before = "removed") |>
        dplyr::filter(origin %in% c("h", "w")) |>
        dplyr::mutate(
          across(
            source,
            str_remove,
            "_h$"),
          across(
            source,
            str_remove,
            "_w$"),
          across(
            source,
            ~ str_to_title(str_replace_all(., "_", " "))
          ),
          across(
            origin,
            recode,
            "h" = "Hatchery",
            "w" = "Natural"
          )
        ) |>
        dplyr::filter(spawn_year %in% query_year,
                      population == "Wenatchee")
    }
    
  } else {
    message("Removal data not found.\n")
    removal_df <- NULL
  }
  
  
  #-----------------------------------------------------------------
  # pull in some estimates from DABOM
  
  message("\t Gathering PIT escapement estimates.\n")
  
  all_escp = readxl::read_excel(paste(dabom_file_path,
                                      dabom_file_name,
                                      sep = "/"),
                                sheet = "Run Escp All Locations") |>
    janitor::clean_names() |>
    dplyr::filter(spawn_year %in% query_year,
                  location %in% c('ICL',
                                  'PES',
                                  'MCL',
                                  'CHM',
                                  'CHW',
                                  'CHL',
                                  'NAL',
                                  'LWN',
                                  'WTL',
                                  'LWE',
                                  'LWE_bb',
                                  'TUM_bb',
                                  'UWE_bb'))
  
  # pull out estimates of tributary spawners from DABOM
  trib_spawners_all = all_escp |>
    dplyr::filter(location %in% c('ICL',
                                  'PES',
                                  'MCL',
                                  'CHM',
                                  'CHW',
                                  'CHL',
                                  'NAL',
                                  'LWN',
                                  'WTL')) |>
    dplyr::select(spawn_year,
                  origin,
                  location,
                  spawners = estimate,
                  spawners_se = se) |>
    dplyr::mutate(
      dplyr::across(origin,
                    recode,
                    "W" = "Natural",
                    "H" = "Hatchery"),
      dplyr::across(location,
                    recode,
                    'CHL' = 'Chiwawa',
                    'CHM' = 'Chumstick',
                    'CHW' = 'Chiwaukum',
                    'ICL' = 'Icicle',
                    'LWN' = 'Little Wenatchee',
                    'MCL' = 'Mission',
                    'NAL' = 'Nason',
                    'PES' = 'Peshastin',
                    'WTL' = 'White River')) |>
    dplyr::arrange(location, origin)
  
  # pull out mainstem escapement estimates
  escp_wen_all = all_escp |>
    dplyr::filter(location %in% c('LWE',
                                  'LWE_bb',
                                  'TUM_bb',
                                  'UWE_bb')) |>
    dplyr::mutate(
      dplyr::across(location,
                    recode,
                    'LWE' = 'Wen_all',
                    'LWE_bb' = 'Below Tumwater',
                    'TUM_bb' = "Above Tumwater",
                    'UWE_bb' = 'Above Tumwater')) |>
    dplyr::mutate(
      dplyr::across(origin,
                    recode,
                    "W" = "Natural",
                    "H" = "Hatchery")) |>
    dplyr::group_by(spawn_year,
                    location,
                    origin) |>
    dplyr::summarise(
      dplyr::across(estimate,
                    sum),
      dplyr::across(se,
                    ~ sqrt(sum(.^2))),
      .groups = "drop")
  
  #-----------------------------------------------------------------
  # save
  if(save_rda & save_by_year) {
    for(yr in query_year) {
      message(paste("Saving data from spawn year",
                    yr,
                    ".\n\n"))
      
      if(!is.null(redd_df_all)) {
        redd_df <- redd_df_all |>
          dplyr::filter(spawn_year == yr)
      } else {
        redd_df <- NULL
      }
      
      wen_tags <- wen_tags_all |>
        dplyr::filter(spawn_year == yr)
      
      sex_err <- sex_err_rate |>
        dplyr::filter(spawn_year == yr)
      
      fpr_df <- fpr_all |>
        dplyr::filter(spawn_year == yr)
      
      trib_spawners <- trib_spawners_all |>
        dplyr::filter(spawn_year == yr)
      
      escp_wen <- escp_wen_all |>
        dplyr::filter(spawn_year == yr)
      
      rem_df <- removal_df |>
        dplyr::filter(spawn_year == yr)
      
      if(is.null(save_file_name)) {
        file_nm = paste0('wen_', yr, '.rda')
      } else {
        file_nm = save_file_name
      }
      
      save(redd_df,
           wen_tags,
           sex_err,
           fpr_df,
           trib_spawners,
           escp_wen,
           rem_df,
           file = paste(save_file_path,
                        file_nm,
                        sep = "/"))
      rm(file_nm)
      
    }
  }
  else {
    if(!is.null(redd_df_all)) {
      redd_df <- redd_df_all
    } else {
      redd_df <- NULL
    }
    
    wen_tags <- wen_tags_all
    
    sex_err <- sex_err_rate
    
    fpr_df <- fpr_all
    
    trib_spawners <- trib_spawners_all
    
    escp_wen <- escp_wen_all
    
    rem_df <- removal_df
    
    
    if(save_rda & !save_by_year) {
      if(is.null(save_file_name)) {
        if(length(query_year) > 1) {
          save_file_name <- paste0('wen_',
                                   paste(min(query_year),
                                         max(query_year),
                                         sep = "-"),
                                   '.rda')
        } else {
          save_file_name <- paste0('wen_',
                                   query_year,
                                   '.rda')
        }
      }
      save(redd_df,
           wen_tags,
           sex_err,
           fpr_df,
           trib_spawners,
           escp_wen,
           rem_df,
           file = paste(save_file_path,
                        save_file_name,
                        sep = "/"))
    } else {
      
      tmp_file <- tempfile(fileext = ".rda")
      
      save(redd_df,
           wen_tags,
           sex_err,
           fpr_df,
           trib_spawners,
           escp_wen,
           rem_df,
           file = tmp_file)
      
      load(tmp_file,
           envir = .GlobalEnv)
      
      file.remove(tmp_file)
      rm(tmp_file)
    }
  }
  
}

######

#-----------------------------------------------------------------
# what year(s) are being prepped
yrs = 2014:2022
# how many observers were used for these surveys?
n_observers = "two"

# gather, prepare, wrangle data, and save relevant pieces as .rda objects
# one for each year
# prep_wen_sthd_data(query_year = yrs,
#                    n_observers = n_observers,
#                    save_rda = T)

prep_wen_sthd_data(
  redd_file_path =
    here("data"),
  redd_file_name = "STHD_Wenatchee_Redd_Surveys.xlsx",
  experience_path = here("data"),
  experience_file_name = "STHD_Surveyor_Experience.xlsx",
  dabom_file_path =
    here("data"),
  dabom_file_name = "UC_STHD_Model_Output.xlsx",
  brood_file_path =
    here("data"),
  brood_file_name = "STHD_UC Brood Collections_2011 to current.xlsx",
  removal_file_path =
    here("data"),
  removal_file_name = "STHD_Removals.xlsx",
  n_observers = n_observers,
  query_year = yrs,
  save_rda = T,
  save_by_year = T,
  save_file_path = here::here("output"),
  save_file_name = "redd_df.rda"
)


