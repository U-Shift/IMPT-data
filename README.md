# IMPT Data

This repository aims to document the data treatment and processing steps for the IMPT (Insert Meaningful Project Title) calculation.

## Data

Due to GitHub data storage restrictions, data is ignored by git control version and is stored instead at ushift@alfa server in `/data/IMPT`.

To access it, use `impt_read(<file path>)` where `<file path>` is relative to `/data/IMPT`.

To store it, use `impt_write(<data>, <file path>)` where `<file path>` is relative to `/data/IMPT`.

> When working locally, these methods automatically detect you are not working at ushift@alfa and read data remotelly from there. Writting will be handled locally and should be uploaded to the server manually. Nevertheless, both methods log information about where the data is being read from or written to and warn you when manual upload is required.

> To access the data remotelly, environment variable `IMPT_SERVER_KEY` is required to be set with 
the authorization key (run `usethis::edit_r_environ()` to set it and make sure to restart R before using it, with CTRL+F10).

### Permissions

When creating a new folder, please make sure to run the command below to set the correct permissions, so that all users can read, write, and execute files within that folder:

```bash
$ chmod 777 /data/IMPT/folderName
```

