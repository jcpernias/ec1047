library(tidyverse)

# Column types for the different ECV files
column_types <- list(
  d = cols(
    .default = 'i',
    DB020 = 'c', DB040 = 'c',
    DB090 = 'd'),
  h = cols(
    .default = 'i',
    HB020 = 'c', HY020 = 'd', HY022 = 'd', HY023 = 'd',
    HY030N = 'd', HY040N = 'd', HY050N = 'd', HY060N = 'd',
    HY070N = 'd', HY080N = 'd', HY081N = 'd', HY090N = 'd',
    HY100N = 'd', HY110N = 'd', HY120N = 'd', HY130N = 'd',
    HY131N = 'd', HY145N = 'd', HY170N = 'd',
    HY010 = 'd',
    HY040G = 'd', HY050G = 'd', HY060G = 'd', HY070G = 'd',
    HY080G = 'd', HY081G = 'd', HY090G = 'd', HY100G = 'd',
    HY110G = 'd', HY120G = 'd', HY130G = 'd', HY131G = 'd',
    HY140G = 'd',
    HS130 = 'd',
    HH060 = 'd', HH061 = 'd', HH070 = 'd',
    cuotahip = 'd', # 2012: recorded with decimal places
    HX240 = 'd',
    vhRentaa = 'd', vhRentaAIa = 'd'),
  r = cols(
    .default = 'i',
    RB020 = 'c', RB050 = 'd',
    RL070 = 'd'),
  p = cols(
    .default = 'i',
    PB020 = 'c', PB040 = 'd', PE020 = 'c', PE040 = 'c',
    PL111A = 'c',
    PY010N = 'd', PY020N = 'd', PY021N = 'd', PY035N = 'd',
    PY050N = 'd', PY080N = 'd', PY090N = 'd', PY100N = 'd',
    PY110N = 'd', PY120N = 'd', PY130N = 'd', PY140N = 'd',
    PY010G = 'd', PY020G = 'd', PY021G = 'd', PY030G = 'd',
    PY035G = 'd', PY050G = 'd', PY080G = 'd', PY090G = 'd',
    PY100G = 'd', PY110G = 'd', PY120G = 'd', PY130G = 'd',
    PY140G = 'd',
    PHD02T = 'd')
)


# Region codes
region_codes <-
  c('AND', 'ARA', 'AST', 'BAL', 'CNR',
    'CNT', 'CYL', 'CLM', 'CAT', 'VAL',
    'EXT', 'GAL', 'MAD', 'MUR', 'NAV',
    'PVA', 'RIO', 'CEU', 'MEL')

region_table <- setNames(
  region_codes,
  c('ES61', 'ES24', 'ES12', 'ES53', 'ES70',
    'ES13', 'ES41', 'ES42', 'ES51', 'ES52',
    'ES43', 'ES11', 'ES30', 'ES62', 'ES22',
    'ES21', 'ES23', 'ES63', 'ES64'))

#' @param year numeric; one of 2008, 2009, ... 2018.
#' @param type 'd', 'h', 'r' or 'p'
#' @noRd
read_ecv_file <- function(year, type) {
  stopifnot(type %in% c('d', 'h', 'r', 'p'))
  stopifnot(year %in% 2008:2018)
  ecv_file <-
    file.path('data-raw',
              paste0('esudb',
                     formatC(year - 2000, width = 2, flag = '0'),
                     type, '.csv.gz'))

    # No HY170N before 2010
  ct <- column_types[[type]]
  if (year < 2010 && type == 'h') {
    ct$cols$HY170N <- NULL
  }

  read_csv(ecv_file, col_types = ct)
}

make_db <- function(year) {
  # Basic household data
  d_file_db <- read_ecv_file(year, 'd')

  # Detailed household data
  h_file_db <- read_ecv_file(year, 'h')

  # Basic individual data
  # r_file_db <- read_ecv_file(year, 'r')

  # Detailed personal data
  # p_file_db <- read_ecv_file(year, 'p')

  # Join household databases
  households <- left_join(d_file_db, h_file_db,
                          by = c('DB030' = 'HB030'))

  region_db <- households %>%
    transmute(hh_id = DB030,
              region = factor(region_table[DB040],
                              levels = region_codes))

  # Select income variables and demographic characteristics
  households %>%
    transmute(hh_id = DB030,
              year = year,
              people = HX040,
              cunits = HX240,
              weight = DB090,
              region = factor(region_table[DB040],
                              levels = region_codes),
              ydisp_hh = vhRentaa,
              ydisp_cu = ydisp_hh / cunits)
}

ecv <- purrr::map_dfr(2008:2018, make_db)

## Store ecv dataset
usethis::use_data(ecv, compress = 'xz', overwrite = TRUE)
