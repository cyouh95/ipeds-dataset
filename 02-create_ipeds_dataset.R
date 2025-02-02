library(tidyverse)
library(readxl)
library(hablar)


ipeds_dir <- 'ipeds'

ipeds_data_dir <- file.path(ipeds_dir, 'data')
ipeds_dict_dir <- file.path(ipeds_dir, 'dictionary')


# ---------------------------
# Select years and variables
# ---------------------------

years <- 2021:2004
year_invariant <- 2009

variables <- list(
  'hd' = list(
    'c' = c(
      'UNITID', 'INSTNM', 'ADDR', 'CITY', 'STABBR', 'ZIP', 'FIPS', 'OBEREG',
      'OPEID', 'OPEFLAG', 'WEBADDR', 'SECTOR', 'ICLEVEL', 'CONTROL',
      'HLOFFER', 'UGOFFER', 'GROFFER', 'HDEGOFR1', 'DEGGRANT',
      'HBCU', 'HOSPITAL', 'MEDICAL', 'TRIBAL', 'LOCALE',
      'OPENPUBL', 'ACT', 'NEWID', 'DEATHYR', 'CLOSEDAT', 'CYACTIVE',
      'POSTSEC', 'PSEFLAG', 'PSET4FLG', 'RPTMTH', 'INSTCAT',
      'C21BASIC', 'C18BASIC', 'C15BASIC', 'CCBASIC', 'CARNEGIE',
      'INSTSIZE', 'F1SYSTYP', 'F1SYSNAM', 'F1SYSCOD',
      'CBSA', 'CBSATYPE', 'CSA', 'COUNTYCD', 'COUNTYNM'
    ),
    'n' = c('LONGITUD', 'LATITUDE')
  ),
  'ic_ay' = list(
    'c' = c('UNITID'),
    'n' = c(
      'CHG2AT2', 'CHG2AF2', 'CHG3AT2', 'CHG3AF2', 'CHG4AY2', 'CHG2AY2',
      'CHG5AY2', 'CHG6AY2', 'CHG7AY2', 'CHG8AY2', 'CHG9AY2', 'CHG3AY2'
    )
  ),
  'efia' = list(
    'c' = c('UNITID'),
    'n' = c(
      'EFTEUG', 'FTEUG'
    )
  ),
  'adm' = list(
    'c' = c('UNITID', 'ADMCON1', 'ADMCON2', 'ADMCON3', 'ADMCON4', 'ADMCON5', 'ADMCON6', 'ADMCON7', 'ADMCON8', 'ADMCON9'),
    'n' = c(
      'APPLCN', 'APPLCNM', 'APPLCNW', 'ADMSSN', 'ADMSSNM', 'ADMSSNW',
      'SATNUM', 'SATPCT', 'ACTNUM', 'ACTPCT', 'SATVR25', 'SATVR75', 'SATMT25', 'SATMT75',
      'ACTCM25', 'ACTCM75', 'ACTEN25', 'ACTEN75', 'ACTMT25', 'ACTMT75'
    )
  ),
  'efa' = list(
    'c' = c('UNITID', 'LINE'),
    'n' = c(
      # Year=2008 and onward
      'EFTOTLT', 'EFTOTLM', 'EFTOTLW',
      'EFAIANT', 'EFAIANM', 'EFAIANW',
      'EFASIAT', 'EFASIAM', 'EFASIAW',
      'EFBKAAT', 'EFBKAAM', 'EFBKAAW',
      'EFHISPT', 'EFHISPM', 'EFHISPW',
      'EFNHPIT', 'EFNHPIM', 'EFNHPIW',
      'EFWHITT', 'EFWHITM', 'EFWHITW',
      'EF2MORT', 'EF2MORM', 'EF2MORW',
      'EFUNKNT', 'EFUNKNM', 'EFUNKNW',
      'EFNRALT', 'EFNRALM', 'EFNRALW',
      
      # Year=2007 and before
      'EFRACE24', 'EFRACE15', 'EFRACE16',  # Grand total
      'EFRACE19', 'EFRACE05', 'EFRACE06',  # American Indian or Alaska Native
      'EFRACE20', 'EFRACE07', 'EFRACE08',  # Asian or Pacific Islander
      'EFRACE18', 'EFRACE03', 'EFRACE04',  # Black non-Hispanic
      'EFRACE21', 'EFRACE09', 'EFRACE10',  # Hispanic
      'EFRACE22', 'EFRACE11', 'EFRACE12',  # White non-Hispanic
      'EFRACE23', 'EFRACE13', 'EFRACE14',  # Race/ethnicity unknown
      'EFRACE17', 'EFRACE01', 'EFRACE02'   # Nonresident alien
    )
  ),
  'efc' = list(
    'c' = c('UNITID', 'EFCSTATE'),
    'n' = c('EFRES01')
  ),
  'efd' = list(
    'c' = c('UNITID'),
    'n' = c('RET_PCF', 'RET_PCP', 'STUFACR')
  ),
  'eap' = list(
    # Year=2012 and onward
    'c' = c('UNITID', 'EAPCAT'),
    'n' = c('EAPFTTYP', 'EAPPTTYP'),
    
    # Year=2011 and before
    'c' = c('EAPRECTP'),
    'n' = c('EAPTYP')
  ),
  'f' = list(
    'c' = c('UNITID'),
    'n' = c(
      # Instruction
      'F1C011', 'F1C012', 'F1C013', 'F1C014', 'F1C015', 'F1C016', 'F1C017',
      
      # Academic support
      'F1C051', 'F1C052', 'F1C053', 'F1C054', 'F1C055', 'F1C056', 'F1C057',
      
      # Student services
      'F1C061', 'F1C062', 'F1C063', 'F1C064', 'F1C065', 'F1C066', 'F1C067',
      
      # Institutional support
      'F1C071', 'F1C072', 'F1C073', 'F1C074', 'F1C075', 'F1C076', 'F1C077'
    )
  ),
  'sfa' = list(
    'c' = c('UNITID'),
    'n' = c(
      # Grant aid
      'AGRNT_N', 'AGRNT_P', 'AGRNT_T', 'AGRNT_A',
      'FGRNT_N', 'FGRNT_P', 'FGRNT_T', 'FGRNT_A',
      'PGRNT_N', 'PGRNT_P', 'PGRNT_T', 'PGRNT_A',
      'OFGRT_N', 'OFGRT_P', 'OFGRT_T', 'OFGRT_A',
      'SGRNT_N', 'SGRNT_P', 'SGRNT_T', 'SGRNT_A',
      'IGRNT_N', 'IGRNT_P', 'IGRNT_T', 'IGRNT_A',
      
      # Loans
      'LOAN_N', 'LOAN_P', 'LOAN_T', 'LOAN_A',
      'FLOAN_N', 'FLOAN_P', 'FLOAN_T', 'FLOAN_A',
      'OLOAN_N', 'OLOAN_P', 'OLOAN_T', 'OLOAN_A',
      
      # Grant and scholarship aid by income band
      'GIS4N2', 'GIS4T2', 'GIS4A2',
      'GIS4N12', 'GIS4T12', 'GIS4A12',
      'GIS4N22', 'GIS4T22', 'GIS4A22',
      'GIS4N32', 'GIS4T32', 'GIS4A32',
      'GIS4N42', 'GIS4T42', 'GIS4A42',
      'GIS4N52', 'GIS4T52', 'GIS4A52',
      
      # Average net price by income band
      'NPIST2', 'NPIS412', 'NPIS422', 'NPIS432', 'NPIS442', 'NPIS452',
      
      # Undergraduate aid, pell-grant, and loans
      'UAGRNTN', 'UAGRNTP', 'UAGRNTT', 'UAGRNTA',
      'UPGRNTN', 'UPGRNTP', 'UPGRNTT', 'UPGRNTA',
      'UFLOANN', 'UFLOANP', 'UFLOANT', 'UFLOANA'
    )
  ),
  'gr' = list(
    'c' = c('UNITID', 'GRTYPE'),
    'n' = c(
      # Year=2008 and onward
      'GRTOTLT', 'GRTOTLM', 'GRTOTLW',
      'GRAIANT', 'GRAIANM', 'GRAIANW',
      'GRASIAT', 'GRASIAM', 'GRASIAW',
      'GRBKAAT', 'GRBKAAM', 'GRBKAAW',
      'GRHISPT', 'GRHISPM', 'GRHISPW',
      'GRNHPIT', 'GRNHPIM', 'GRNHPIW',
      'GRWHITT', 'GRWHITM', 'GRWHITW',
      'GR2MORT', 'GR2MORM', 'GR2MORW',
      'GRUNKNT', 'GRUNKNM', 'GRUNKNW',
      'GRNRALT', 'GRNRALM', 'GRNRALW',
      
      # Year=2007 and before
      'GRRACE24', 'GRRACE15', 'GRRACE16',  # Grand total
      'GRRACE19', 'GRRACE05', 'GRRACE06',  # American Indian or Alaska Native
      'GRRACE20', 'GRRACE07', 'GRRACE08',  # Asian or Pacific Islander
      'GRRACE18', 'GRRACE03', 'GRRACE04',  # Black non-Hispanic
      'GRRACE21', 'GRRACE09', 'GRRACE10',  # Hispanic
      'GRRACE22', 'GRRACE11', 'GRRACE12',  # White non-Hispanic
      'GRRACE23', 'GRRACE13', 'GRRACE14',  # Race/ethnicity unknown
      'GRRACE17', 'GRRACE01', 'GRRACE02'   # Nonresident alien
    )
  ),
  'gr_pell' = list(
    'c' = c('UNITID', 'PSGRTYPE'),
    'n' = c(
      # Pell Grant recipients
      'PGCMBAC', 'PGCMOBA', 'PGCMTOT',
      
      # Subsidized Stafford loan recipients who did not receive a pell grant
      'SSCMBAC', 'SSCMOBA', 'SSCMTOT'
    )
  ),
  'gr200' = list(
    'c' = c('UNITID'),
    'n' = c(
      # 4-year institutions
      'BAGR100', 'BAGR150', 'BAGR200',
      
      # 100% for 2-year or less institutions
      'L4NC100', 'L4GR100', 
      
      # 150% for 2-year or less institutions
      'L4AC150', 'L4NC150', 'L4GR150',
      
      # 200% for 2-year or less institutions
      'L4AC200', 'L4NC200A', 'L4NC200', 'L4GR200'
    )
  )
)

var_list <- list()

for (survey in names(variables)) {
  x <- c()
  for(idx in seq(names(variables[[survey]]))) {
    k <- variables[[survey]][[idx]]
    v <- rep(names(variables[[survey]])[[idx]], length(k))
    
    # Save both upper and lowercase version of variable names to accommodate different years
    x <- c(x, setNames(v, k), setNames(v, str_to_lower(k)))
  }
  var_list[[survey]] <- as.list(x)
}


# -------------------------------
# Save variable and value labels
# -------------------------------

files <- list(
  'hd' = 'hd%s',
  'ic_ay' = 'ic%s_ay',
  'efia' = 'efia%s',
  'adm' = 'adm%s',
  'efa' = 'ef%sa',
  'efc' = 'ef%sc',
  'efd' = 'ef%sd',
  'eap' = 'eap%s',
  'f' = 'f%s_f1a',
  'sfa' = 'sfa%s',
  'gr' = 'gr%s',
  'gr_pell' = 'gr%s_pell_ssl',
  'gr200' = 'gr200_%s'
)

get_filename <- function(yr, survey) {
  if (survey %in% c('f', 'sfa')) {
    yr <- str_c(str_sub(yr - 1, 3, 4), str_sub(yr, 3, 4))
  } else if (survey == 'gr200') {
    yr <- str_sub(yr, 3, 4)
  }
  
  sprintf(if_else(survey == 'adm' && yr <= 2013, 'ic%s', files[[survey]]), yr)
}


# Variable labels

vars_dfs <- lapply(years, function(yr) {
  surveys <- lapply(names(variables), function(survey) {
    
    filename <- list.files(ipeds_dict_dir, str_c(get_filename(yr, survey), '.x'))
    
    if (length(filename) == 1) {
      sheet_name <- if_else('varlist' %in% excel_sheets(file.path(ipeds_dict_dir, filename)), 'varlist', 'Varlist')
      dict_vars <- read_excel(file.path(ipeds_dict_dir, filename), sheet = sheet_name)
      
      var_names <- names(var_list[[survey]])
      
      dict_vars %>%
        select(varname, varTitle) %>%
        filter(varname %in% var_names, varname != 'UNITID')
    } else {
      writeLines(str_c('No ', survey, ' dictionary found for year: ', yr))
    }
  })
  
  do.call(bind_rows, surveys) %>%
    mutate(YEAR = yr) %>%
    pivot_wider(id_cols = 'YEAR', names_from = 'varname', values_from = 'varTitle')
})

var_order <- grep('^[^a-z]+$', names(flatten(var_list)), value = T)
var_order <- c('YEAR', var_order[var_order != 'UNITID'])

labels_var <- do.call(bind_rows, vars_dfs) %>% 
  select(all_of(var_order))

write_csv(labels_var, 'labels_var.csv')


# Value labels

vals_dfs <- lapply(years, function(yr) {
  surveys <- lapply(names(variables), function(survey) {
    
    filename <- list.files(ipeds_dict_dir, str_c(get_filename(yr, survey), '.x'))
    
    if (length(filename) == 1) {
      if ('Frequencies' %in% excel_sheets(file.path(ipeds_dict_dir, filename))) {
        dict_vals <- read_excel(file.path(ipeds_dict_dir, filename), sheet = 'Frequencies')
        
        var_names <- names(var_list[[survey]])
        
        dict_vals %>%
          select(varname, codevalue, valuelabel) %>% 
          filter(varname %in% var_names)
      } else {
        writeLines(str_c('No frequencies tab in: ', filename))
      }
    } else {
      writeLines(str_c('No ', survey, ' dictionary found for year: ', yr))
    }
  })
  
  do.call(bind_rows, surveys) %>%
    mutate(YEAR = yr)
})

labels_val <- do.call(bind_rows, vals_dfs) %>%
  pivot_wider(id_cols = c('varname', 'codevalue'), names_from = 'YEAR', values_from = 'valuelabel') %>% 
  arrange(varname, codevalue)

write_csv(labels_val, 'labels_val.csv')


# -------------------
# Read in data files
# -------------------

load_data <- function(years, survey) {
  all_dfs <- lapply(years, function(yr) {
    
    fn <- get_filename(yr, survey)
    suf <- if_else(str_c(fn, '_rv.csv') %in% list.files(ipeds_data_dir), '_rv.csv', '.csv')
    
    filename <- file.path(ipeds_data_dir, str_c(fn, suf))
    if (file.exists(filename)) {
      d <- read_csv(
        filename,
        col_types = do.call(cols_only, var_list[[survey]]),
        na = c('.', '')
      ) %>% 
        rename_with(str_to_upper)
      
      # Rename variables from older datasets
      if (survey == 'efa' && yr <= 2007) {
        d <- d %>% 
          rename(
            'EFTOTLT' = 'EFRACE24', 'EFTOTLM' = 'EFRACE15', 'EFTOTLW' = 'EFRACE16',  # Grand total
            'EFAIANT' = 'EFRACE19', 'EFAIANM' = 'EFRACE05', 'EFAIANW' = 'EFRACE06',  # American Indian or Alaska Native
            'EFASPIT' = 'EFRACE20', 'EFASPIM' = 'EFRACE07', 'EFASPIW' = 'EFRACE08',  # Asian or Pacific Islander
            'EFBKAAT' = 'EFRACE18', 'EFBKAAM' = 'EFRACE03', 'EFBKAAW' = 'EFRACE04',  # Black non-Hispanic
            'EFHISPT' = 'EFRACE21', 'EFHISPM' = 'EFRACE09', 'EFHISPW' = 'EFRACE10',  # Hispanic
            'EFWHITT' = 'EFRACE22', 'EFWHITM' = 'EFRACE11', 'EFWHITW' = 'EFRACE12',  # White non-Hispanic
            'EFUNKNT' = 'EFRACE23', 'EFUNKNM' = 'EFRACE13', 'EFUNKNW' = 'EFRACE14',  # Race/ethnicity unknown
            'EFNRALT' = 'EFRACE17', 'EFNRALM' = 'EFRACE01', 'EFNRALW' = 'EFRACE02'   # Nonresident alien
          )
      } else if (survey == 'gr' && yr <= 2007) {
        d <- d %>% 
          rename(
            'GRTOTLT' = 'GRRACE24', 'GRTOTLM' = 'GRRACE15', 'GRTOTLW' = 'GRRACE16',  # Grand total
            'GRAIANT' = 'GRRACE19', 'GRAIANM' = 'GRRACE05', 'GRAIANW' = 'GRRACE06',  # American Indian or Alaska Native
            'GRASPIT' = 'GRRACE20', 'GRASPIM' = 'GRRACE07', 'GRASPIW' = 'GRRACE08',  # Asian or Pacific Islander
            'GRBKAAT' = 'GRRACE18', 'GRBKAAM' = 'GRRACE03', 'GRBKAAW' = 'GRRACE04',  # Black non-Hispanic
            'GRHISPT' = 'GRRACE21', 'GRHISPM' = 'GRRACE09', 'GRHISPW' = 'GRRACE10',  # Hispanic
            'GRWHITT' = 'GRRACE22', 'GRWHITM' = 'GRRACE11', 'GRWHITW' = 'GRRACE12',  # White non-Hispanic
            'GRUNKNT' = 'GRRACE23', 'GRUNKNM' = 'GRRACE13', 'GRUNKNW' = 'GRRACE14',  # Race/ethnicity unknown
            'GRNRALT' = 'GRRACE17', 'GRNRALM' = 'GRRACE01', 'GRNRALW' = 'GRRACE02'   # Nonresident alien
          )
      } else if (survey == 'eap' && yr <= 2011) {
        # Currently only supports full-time and part-time instructional staff (EAPCAT='21000')
        d <- d %>%
          filter(EAPRECTP %in% c('2230', '3230')) %>%
          mutate(
            FTPT = recode(EAPRECTP, '2230' = 'FT', '3230' = 'PT'),
            EAPCAT = '21000'
          ) %>%
          pivot_wider(
            id_cols = c('UNITID', 'EAPCAT'),
            names_from = 'FTPT',
            names_glue = 'EAP{FTPT}TYP',
            values_from = 'EAPTYP'
          )
      }
      
      d[str_c(str_to_upper(survey), '_SOURCE_FILE')] <- str_c(fn, suf)
      cbind(YEAR = yr, d)
    } else {
      writeLines(str_c('File not found: ', filename))
    }
  })
  
  ordered_vars <- grep('^[^a-z]+$', names(var_list[[survey]]), value = T)
  if (survey == 'efa') {
    ordered_vars <- c(ordered_vars[!str_detect(ordered_vars, 'EFRACE')], 'EFASPIT', 'EFASPIM', 'EFASPIW')
  } else if (survey == 'gr') {
    ordered_vars <- c(ordered_vars[!str_detect(ordered_vars, 'GRRACE')], 'GRASPIT', 'GRASPIM', 'GRASPIW')
  } else if (survey == 'eap') {
    ordered_vars <- ordered_vars[!ordered_vars %in% c('EAPRECTP', 'EAPTYP')]
  }
  
  do.call(bind_rows, all_dfs) %>% 
    select(all_of(c('YEAR', ordered_vars)), ends_with('_SOURCE_FILE'))
}

# HD
hd <- load_data(years, 'hd')
hd <- hd %>% 
  add_column(
    OPEID5 = if_else(hd$OPEID == '-2', NA_character_, str_sub(hd$OPEID, 2, 6)),
    OPEIDSUF = if_else(hd$OPEID == '-2', NA_character_, str_sub(hd$OPEID, 7, 8)),
    .after = 'OPEID'
  )

# IC
ic_ay <- load_data(years, 'ic_ay')

# EFIA
efia <- load_data(years, 'efia')

# ADM
adm <- load_data(years, 'adm')
adm <- adm %>% 
  add_column(SATCM25 = adm$SATVR25 + adm$SATMT25, SATCM75 = adm$SATVR75 + adm$SATMT75, .after = 'SATMT75')

# EFA
efa <- load_data(years, 'efa') %>% 
  filter(LINE %in% c('1', '2', '6', '8', '15', '16', '20', '22', '29')) %>% 
  pivot_wider(
    id_cols = c('YEAR', 'UNITID', 'EFA_SOURCE_FILE'),
    names_from = LINE,
    values_from = -c('YEAR', 'UNITID', 'LINE', 'EFA_SOURCE_FILE'),
    names_vary = 'slowest'
  ) %>% 
  relocate(EFA_SOURCE_FILE, .after = last_col())

# EFC
efc <- load_data(years, 'efc') %>%
  filter(!EFCSTATE %in% c('58', '89', '99')) %>%
  left_join(hd %>% select(YEAR, UNITID, FIPS), by = c('YEAR', 'UNITID')) %>% 
  mutate(locale = if_else(EFCSTATE == '98', 'UNKNOWN', if_else(EFCSTATE == FIPS, 'INSTATE', 'OUTOFSTATE'))) %>% 
  group_by(YEAR, UNITID, EFC_SOURCE_FILE, locale) %>% 
  summarise(count = sum_(EFRES01), .groups = 'drop') %>% 
  pivot_wider(
    id_cols = c('YEAR', 'UNITID', 'EFC_SOURCE_FILE'),
    names_from = locale,
    names_prefix = 'NUM_',
    values_from = count,
    values_fill = list(count = 0)
  ) %>% 
  mutate(
    NUM_TOTAL = NUM_INSTATE + NUM_OUTOFSTATE + NUM_UNKNOWN,
    denominator = NUM_INSTATE + NUM_OUTOFSTATE,
    PCT_INSTATE = NUM_INSTATE / denominator,
    PCT_OUTOFSTATE = NUM_OUTOFSTATE / denominator
  ) %>% 
  select(-denominator) %>% 
  relocate(EFC_SOURCE_FILE, .after = last_col())

# EFD
efd <- load_data(years, 'efd')

# EAP
eap <- load_data(years, 'eap') %>% 
  filter(EAPCAT == '21000') %>%  # currently only supports full-time and part-time instructional staff
  pivot_wider(
    id_cols = c('YEAR', 'UNITID', 'EAP_SOURCE_FILE'),
    names_from = EAPCAT,
    values_from = c('EAPFTTYP', 'EAPPTTYP'),
    names_vary = 'slowest'
  ) %>% 
  relocate(EAP_SOURCE_FILE, .after = last_col())

# F
f <- load_data(years, 'f')

# SFA
sfa <- load_data(years, 'sfa')

# GR
gr <- load_data(years, 'gr') %>% 
  filter(GRTYPE %in% c('8', '12', '20', '21', '29', '30', '33')) %>% 
  pivot_wider(
    id_cols = c('YEAR', 'UNITID', 'GR_SOURCE_FILE'),
    names_from = GRTYPE,
    values_from = -c('YEAR', 'UNITID', 'GRTYPE', 'GR_SOURCE_FILE'),
    names_vary = 'slowest'
  ) %>% 
  relocate(GR_SOURCE_FILE, .after = last_col())

# GR_PELL
gr_pell <- load_data(years, 'gr_pell') %>% 
  pivot_wider(
    id_cols = c('YEAR', 'UNITID', 'GR_PELL_SOURCE_FILE'),
    names_from = PSGRTYPE,
    values_from = -c('YEAR', 'UNITID', 'PSGRTYPE', 'GR_PELL_SOURCE_FILE'),
    names_vary = 'slowest'
  ) %>% 
  relocate(GR_PELL_SOURCE_FILE, .after = last_col())

# GR200
gr200 <- load_data(years, 'gr200')


# --------------------------------------
# Merge and save YEAR-UNITID-level data
# --------------------------------------

hd_invariant <- hd %>%
  filter(YEAR >= year_invariant) %>% 
  arrange(YEAR) %>% 
  group_by(UNITID) %>% 
  summarise_all(first)

hd_names <- names(hd_invariant)
names(hd_invariant) <- c('UNITID', str_c(hd_names[hd_names != 'UNITID'], '_INVARIANT'))

ipeds <- hd %>% 
  left_join(hd_invariant, by = 'UNITID') %>% 
  left_join(ic_ay, by = c('YEAR', 'UNITID')) %>%
  left_join(efia, by = c('YEAR', 'UNITID')) %>% 
  left_join(adm, by = c('YEAR', 'UNITID')) %>%
  left_join(efa, by = c('YEAR', 'UNITID')) %>% 
  left_join(efc, by = c('YEAR', 'UNITID')) %>% 
  left_join(efd, by = c('YEAR', 'UNITID')) %>% 
  left_join(eap, by = c('YEAR', 'UNITID')) %>% 
  left_join(f, by = c('YEAR', 'UNITID')) %>% 
  left_join(sfa, by = c('YEAR', 'UNITID')) %>% 
  left_join(gr, by = c('YEAR', 'UNITID')) %>% 
  left_join(gr_pell, by = c('YEAR', 'UNITID')) %>% 
  left_join(gr200, by = c('YEAR', 'UNITID')) %>% 
  mutate(
    COMPUTED_STUFACR = (EFTOTLT_8 + 1/3 * EFTOTLT_22) / (EAPFTTYP_21000 + 1/3 * EAPPTTYP_21000),
    FACPER100STU = 100 * (EAPFTTYP_21000 + 1/3 * EAPPTTYP_21000) / (EFTOTLT_8 + 1/3 * EFTOTLT_22)
  )

write_csv(ipeds, file = 'ipeds.csv')


# -----------------------------------------
# Collapse and save YEAR-OPEID5-level data
# -----------------------------------------

opeid5 <- ipeds %>%
  filter(!is.na(OPEID5)) %>%
  arrange(YEAR, OPEID5, OPEIDSUF, OPEFLAG) %>%
  group_by(YEAR, OPEID5) %>%
  summarise(
    NUM_INST = n(),
    ALL_SECTOR = str_c(unique(SECTOR), collapse = '|'),
    ALL_CCBASIC = str_c(unique(CCBASIC), collapse = '|'),
    ALL_CARNEGIE = str_c(unique(CARNEGIE), collapse = '|'),
    across(UNITID:HD_SOURCE_FILE_INVARIANT, first),
    across(EFTEUG:FTEUG, sum_),
    EFIA_SOURCE_FILE = first(EFIA_SOURCE_FILE),
    across(EFTOTLT_29:EFASPIW_16, sum_),
    EFA_SOURCE_FILE = first(EFA_SOURCE_FILE),
    across(NUM_INSTATE:NUM_TOTAL, sum_),
    EFC_SOURCE_FILE = first(EFC_SOURCE_FILE),
    across(EAPFTTYP_21000:EAPPTTYP_21000, sum_),
    EAP_SOURCE_FILE = first(EAP_SOURCE_FILE),
    across(F1C011:F1C077, sum_),
    F_SOURCE_FILE = first(F_SOURCE_FILE),
    across(grep('T$|N$|GIS4N|GIS4T', variables$sfa$n, value = T), sum_),
    SFA_SOURCE_FILE = first(SFA_SOURCE_FILE),
    across(GRTOTLT_8:GRASPIW_33, sum_),
    GR_SOURCE_FILE = first(GR_SOURCE_FILE),
    across(PGCMBAC_1:SSCMTOT_4, sum_),
    GR_PELL_SOURCE_FILE = first(GR_PELL_SOURCE_FILE),
    across(c('L4NC100', 'L4AC150', 'L4NC150', 'L4AC200', 'L4NC200A', 'L4NC200'), sum_),
    GR200_SOURCE_FILE = first(GR200_SOURCE_FILE),
    .groups = 'drop'
  ) %>% 
  mutate(
    COMPUTED_STUFACR = if_else(
      (EFTOTLT_8 == 0 & EFTOTLT_22 == 0 ) | (EAPFTTYP_21000 == 0 & EAPPTTYP_21000 == 0),
      NA_real_,
      (EFTOTLT_8 + 1/3 * EFTOTLT_22) / (EAPFTTYP_21000 + 1/3 * EAPPTTYP_21000)
    ),
    FACPER100STU = if_else(
      (EFTOTLT_8 == 0 & EFTOTLT_22 == 0 ) | (EAPFTTYP_21000 == 0 & EAPPTTYP_21000 == 0),
      NA_real_,
      100 * (EAPFTTYP_21000 + 1/3 * EAPPTTYP_21000) / (EFTOTLT_8 + 1/3 * EFTOTLT_22)
    )
  )

write_csv(opeid5, file = 'opeid5.csv')
