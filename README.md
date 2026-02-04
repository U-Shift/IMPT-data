# IMPT Data

This repository aims to document the data treatment and processing steps for the IMPT (Insert Meaningful Project Title) calculation.

## Data 

## Load

Please use `data_load.R` to load the already preprocessed data.

### Access

Data should be stored in the `/data/IMPT` directory at `ushift@alfa` server. 
For this reason, the usage of https://server.ushift.pt/rstudio/ is recommended to work with this repository.

To run locally, use `IMPT_URL(<file path>)`, with a file path relative to `/data/IMPT`. This method is defined at `code/data_load.R`
and allows to read data directly from the remote server. It requires the environment variable `IMPT_SERVER_KEY` to be set with 
the authorization key (run `usethis::edit_r_environ()` to set it and make sure to restart R before using it, with CTRL+F10).

If you run locally, please make sure to upload relevant outputs to the remote server!

### Permissions

When creating a new folder, please make sure to run the command below to set the correct permissions, so that all users can read, write, and execute files within that folder:

```bash
$ chmod 777 /data/IMPT/folderName
```

