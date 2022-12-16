sheet <- readxl::excel_sheets('data/covid_example_data/covid_example_data.xlsx')
covid <- readxl::read_xlsx('data/covid_example_data/covid_example_data.xlsx', sheet = sheet)
str(covid)

# Missing values for symptoms start = 37480
covid |>
     dplyr::count(is.na(sym_startdt_FALSE))

# Constrict symptom start dates from january 2020 to today ----
covid_cl <- covid |>
     dplyr::filter(!is.na(sym_startdt_FALSE)) |>
     dplyr::filter(sym_startdt_FALSE >= lubridate::dmy('01-01-2020'), sym_startdt_FALSE <= Sys.Date())



covid_cl |>
     dplyr::group_by(lubridate::year(lubridate::floor_date(sym_startdt_FALSE, 'week')), lubridate::week(lubridate::floor_date(sym_startdt_FALSE, 'week'))) |>
     dplyr::count() |>
     dplyr::rename(year = 'lubridate::year(lubridate::floor_date(sym_startdt_FALSE, "week"))', week = 'lubridate::week(lubridate::floor_date(sym_startdt_FALSE, "week"))') |>
     dplyr::arrange(year, week) |>
     dplyr::mutate(yweek = paste(year, '-', week, sep = '')) |>
     dplyr::mutate(yweek = forcats::as_factor(yweek)) |>
     plotly::plot_ly() |>
     plotly::ungroup() |>
     plotly::add_lines(x = ~yweek, name = 'year - week', y = ~n, name = 'Cases') |>
     plotly::layout(title = 'COVID-19 cases 2020 - 2021',
                    xaxis = list(title = 'year-week'),
                    yaxis = list(title = 'Cases'))
