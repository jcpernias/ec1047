library(tidyverse)

# Basic household data
d_file_db <-
  read_csv(file.path('data-raw', 'esudb18d.csv.gz'),
           col_types = cols(
             .default = 'i',
             DB020 = 'c', DB040 = 'c',
             DB090 = 'd'))

# Detailed household data
h_file_db <-
  read_csv(file.path('data-raw', 'esudb18h.csv.gz'),
           col_types = cols(
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
             HX240 = 'd',
             vhRentaa = 'd', vhRentaAIa = 'd'))

# Basic individual data
r_file_db <-
  read_csv(file.path('data-raw', 'esudb18r.csv.gz'),
           col_types = cols(
             .default = 'i',
             RB020 = 'c', RB050 = 'd',
             RL070 = 'd'))

# Detailed personal data
p_file_db <-
  read_csv(file.path('data-raw', 'esudb18p.csv.gz'),
           col_types = cols(
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
             PHD02T = 'd'))

# Join household databases
households <- d_file_db %>%
  left_join(h_file_db, by = c('DB030' = 'HB030'))

# Join data for adults
adults <- r_file_db %>%
  right_join(p_file_db, by = c('RB030' = 'PB030'))

# Isolate data for children
children <- r_file_db %>%
  anti_join(p_file_db, by = c('RB030' = 'PB030'))

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

region_db <- households %>%
  transmute(hh_id = DB030,
            region = factor(region_table[DB040],
                            levels = region_codes))

# Build age and gender variables
gender_codes <- c('M', 'F')

gender_age_db <- r_file_db %>%
  transmute(hh_id = as.integer(RB030 / 100),
            gender = factor(gender_codes[RB090],
                            gender_codes),
            age = cut(2018 - RB080,
                      breaks = c(0, 16, 30, 45, 65, 90),
                      labels = c('age_15', 'age_16_29', 'age_30_44',
                                 'age_45_64', 'age_65'),
                      right = FALSE)) %>%
  group_by(hh_id) %>%
  summarise(f_age_15    = sum(gender == 'F' & age == 'age_15'),
            f_age_16_29 = sum(gender == 'F' & age == 'age_16_29'),
            f_age_30_44 = sum(gender == 'F' & age == 'age_30_44'),
            f_age_45_64 = sum(gender == 'F' & age == 'age_45_64'),
            f_age_65    = sum(gender == 'F' & age == 'age_65'),
            m_age_15    = sum(gender == 'M' & age == 'age_15'),
            m_age_16_29 = sum(gender == 'M' & age == 'age_16_29'),
            m_age_30_44 = sum(gender == 'M' & age == 'age_30_44'),
            m_age_45_64 = sum(gender == 'M' & age == 'age_45_64'),
            m_age_65    = sum(gender == 'M' & age == 'age_65')) %>%
  mutate(
    men   = m_age_15 + m_age_16_29 + m_age_30_44 + m_age_45_64 + m_age_65,
    women = f_age_15 + f_age_16_29 + f_age_30_44 + f_age_45_64 + f_age_65,
    age_15    = m_age_15 + f_age_15,
    age_16_29 = m_age_16_29 + f_age_16_29,
    age_30_44 = m_age_30_44 + f_age_30_44,
    age_45_64 = m_age_45_64 + f_age_45_64,
    age_65    = m_age_65 + f_age_65)

# Select income variables and demographic characteristics
ecv2018 <- households %>%
  select(hh_id = DB030,
         people = HX040,
         cunits = HX240,
         weight = DB090,
         ydisp_hh = vhRentaa) %>%
  mutate(ydisp_cu = ydisp_hh / cunits) %>%
  left_join(gender_age_db, by = 'hh_id') %>%
  left_join(region_db, by = 'hh_id')

## Store ecv2018 dataset
usethis::use_data(ecv2018, compress = 'xz',
                  overwrite = TRUE, version = 3)
