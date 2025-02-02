# IPEDS dataset

Scripts to download IPEDS data and create longitudinal dataset.

## Download IPEDS files

1. Download **downloadipeds.R** and **ipeds_file_list.txt** from Benjamin Skinner's [downloadipeds](https://github.com/btskinner/downloadipeds) and place in project directory
2. Update `out_dir` on line 61 of **downloadipeds.R** as needed (e.g., `out_dir = 'ipeds'`)
3. Edit **ipeds_file_list.txt** to include desired files (see [IPEDS files](https://nces.ed.gov/ipeds/datacenter/datafiles.aspx))
4. Run **01-download_ipeds_files.R** to download and unzip data and dictionary files
