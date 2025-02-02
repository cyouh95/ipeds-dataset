# ---------------------
# Download IPEDS files
# ---------------------

source('downloadipeds.R')


# ------------------
# Unzip IPEDS files
# ------------------

ipeds <- readLines('ipeds_file_list.txt')
ipeds <- ipeds[ipeds != '' & !grepl('^#', ipeds)]

ipeds_data_dir <- file.path('ipeds', 'data')
ipeds_dict_dir <- file.path('ipeds', 'dictionary')

for (i in ipeds) {
  unzip(zipfile = file.path(ipeds_data_dir, paste0(i, '.zip')), exdir = ipeds_data_dir)
  unzip(zipfile = file.path(ipeds_dict_dir, paste0(i, '_Dict.zip')), exdir = ipeds_dict_dir)
}
