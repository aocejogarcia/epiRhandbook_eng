pacman::p_load(tidyverse,
               lubridate,
               plotly)

sheet <- readxl::excel_sheets('data/covid_example_data/covid_example_data.xlsx')
covid <- readxl::read_xlsx('data/covid_example_data/covid_example_data.xlsx', sheet = sheet)
str(covid)

#var	                         label
#PID	                         Personal ID
#reprt_creationdt_FALSE	     Report creation date
#case_dob_FALSE	          Case date of birth
#case_age	                    Case age
#case_gender	               Case gender
#case_race	               Case race
#case_eth	                    Case ethnicity
#case_zip	                    Case zip code
#Contact_id	               Contact ID
#sym_startdt_FALSE	          Sympstoms start date
#sym_fever	               Fever
#sym_subjfever	               Subjective fever
#sym_myalgia	               Myalgia
#sym_losstastesmell	          Loss of taste and smell
#sym_sorethroat	          Sorethroath
#sym_cough	               Cough
#sym_headache	               Headache
#sym_resolved	               Symtoms resolved
#sym_resolveddt_FALSE	     Symtoms resolved date
#contact_household	          Contact household
#hospitalized	               Hospitalized
#hosp_admidt_FALSE	          Hospital admission date
#hosp_dischdt_FALSE	          Hospital discharge date
#died	                    Died
#died_covid	               Died of COVID
#died_dt_FALSE	               Death date
#confirmed_case	          Confirmed case
#covid_dx	                    COVID diagnose
#pos_sampledt_FALSE	          Positive sample date
#latitude_JITT	               Latitude
#longitud_JITT	               Longitud

# Missing values for symptoms start = 37480
covid %>%
     dplyr::count(is.na(sym_startdt_FALSE))

# Constrict symptom start dates from january 2020 to today ----
covid_cl <- covid %>%
     dplyr::filter(!is.na(sym_startdt_FALSE)) %>%
     dplyr::filter(sym_startdt_FALSE >= lubridate::dmy('01-01-2020'), sym_startdt_FALSE <= Sys.Date()) %>%
     dplyr::distinct(PID, .keep_all = TRUE)

covid_cl %>%
     dplyr::group_by(lubridate::year(lubridate::floor_date(sym_startdt_FALSE, 'week')), lubridate::week(lubridate::floor_date(sym_startdt_FALSE, 'week'))) %>%
     dplyr::count() %>%
     dplyr::rename(year = 'lubridate::year(lubridate::floor_date(sym_startdt_FALSE, "week"))', week = 'lubridate::week(lubridate::floor_date(sym_startdt_FALSE, "week"))') %>%
     dplyr::arrange(year, week) %>%
     dplyr::mutate(yweek = paste(year, '-', week, sep = '')) %>%
     dplyr::mutate(yweek = forcats::as_factor(yweek)) %>%
     plotly::plot_ly() %>%
     plotly::ungroup() %>%
     plotly::add_lines(x = ~yweek, name = 'year - week', y = ~n, name = 'Cases') %>%
     plotly::layout(title = 'COVID-19 cases 2020 - 2021',
                    xaxis = list(title = 'year-week'),
                    yaxis = list(title = 'Cases'))
