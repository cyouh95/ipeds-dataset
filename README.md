# IPEDS dataset

Scripts to download IPEDS data and create longitudinal dataset.

## Download IPEDS files

1. Download **downloadipeds.R** and **ipeds_file_list.txt** from Benjamin Skinner's [downloadipeds](https://github.com/btskinner/downloadipeds) and place in project directory
2. Update `out_dir` on line 61 of **downloadipeds.R** as needed (e.g., `out_dir = 'ipeds'`)
3. Edit **ipeds_file_list.txt** to include desired files (see [IPEDS files](https://nces.ed.gov/ipeds/datacenter/datafiles.aspx))
4. Run **01-download_ipeds_files.R** to download and unzip data and dictionary files

## Create IPEDS dataset

Update the following objects as needed in **02-create_ipeds_dataset.R**:

- `ipeds_dir`: Set to the same directory specified by `out_dir` in **downloadipeds.R** above
- `years`: Set to range of years desired in dataset
- `year_invariant`: Set to year to pull time-invariant variables from (HD variables only)
  - _This will pull data from `year_invariant` if available, otherwise the next closest year up_

Run **02-create_ipeds_dataset.R**, which will create the following outputs:

- **label_vars.csv**: Variable name labels over the years
  - _Labels for 2009 HD variables are currently not included since its data dictionary is not in a readily parseable Excel format_
- **label_vals.csv**: Variable value labels over the years
- **ipeds.csv**: YEAR-UNITID-level dataset
- **opeid5.csv**: YEAR-OPEID5-level dataset

Currently, only select IPEDS tables and variables are supported.
