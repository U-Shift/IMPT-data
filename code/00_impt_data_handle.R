library(dplyr)
library(sf)
library(tidyr)


# IMPORTANT! Methods to handle IMPT data  ----------------------------------------------------------
# This methods handle the logic to load/store IMPT data. They should be used to handle all files
# They handle internally all the logic to decide where to gather/store the data, deppending on the machine you are running the code at :)

impt_read <- function(path, root = NULL) {
    # If does not start with "/", add it
    if (!startsWith(path, "/")) {
        path <- paste0("/", path)
    }
    # Get machine id with command cat /etc/machine-id
    machine_id <- system("cat /etc/machine-id", intern = TRUE)
    # If not forcing base path, set value depending on machine
    path_root <- root
    path_append <- ""
    if (is.null(path_root)) {
        # If running at ushift@alfa, read from /data folder
        if (machine_id == "a8709380ae1e4b72904a93456ccfb874") {
            path_root <- "/data"
        } else {
            path_root <- "https://impt.server.ushift.pt"
            if (Sys.getenv("IMPT_DATA_KEY") == "") {
                stop("You are working outside ushift@alfa. To read its content, make sure to have IMPT_DATA_KEY env key defined!\nRefer to https://github.com/U-Shift/IMPT-data/blob/main/README.md README for more details :)")
            }
            path_append <- sprintf("?key=%s", Sys.getenv("IMPT_DATA_KEY"))
        }
    }
    # Read file and return
    file_path <- paste(path_root, path, path_append, sep = "")
    message(sprintf("Downloading file from %s...", file_path))
    if (grepl(".csv", file_path, ignore.case = TRUE)) {
        return(read.csv(file_path))
    }
    if (grepl(".geojson", file_path, ignore.case = TRUE) | grepl(".gpkg", file_path, ignore.case = TRUE)) {
        return(sf::st_read(file_path))
    }
    if (grepl(".rds", file_path, ignore.case = TRUE) | grepl(".Rds", file_path, ignore.case = TRUE)) {
        if (grepl("^http", file_path, ignore.case = TRUE)) {
            file_local <- file.path(tempdir(), basename(path))
            download.file(file_path, file_local, quiet = TRUE, mode = "wb")
            file_path <- file_local
        }
        return(readRDS(file_path))
    }
    # If got here, throw error, as file format is not supported
    stop("Invalid file extension! Consult the IMPT team to improve this method!")
}
impt_write <- function(content, path, root = NULL) {
    # If does not start with "/", add it
    if (!startsWith(path, "/")) {
        path <- paste0("/", path)
    }
    # Get machine id with command cat /etc/machine-id
    machine_id <- system("cat /etc/machine-id", intern = TRUE)
    # If not forcing base path, set value depending on machine
    path_root <- root
    path_append <- ""
    if (is.null(path_root)) {
        # If running at ushift@alfa, read from /data folder
        if (machine_id == "a8709380ae1e4b72904a93456ccfb874") {
            path_root <- "/data"
        } else {
            path_root <- "data/"
            current_location <- getwd()
            warning(sprintf("You are working outside ushift@alfa!\nData will be stored locally inside the data folder of this repo, at %s/data.\nMind that this folder is not synced with GitHub. You should manually upload it to ushift@alfa:/data/IMPT using https://server.ushift.pt/rstudio/", current_location))
        }
    }
    # Read file and return
    file_path <- paste(path_root, path, path_append, sep = "")
    # Get folder from file_path
    file_folder <- os.path.dirname(file_path)
    if (!dir.exists(file_folder)) {
        dir.create(file_folder, recursive = TRUE)
    }
    message(sprintf("Writing file to %s...", file_path))
    # If file exists, warn
    if (os.path.exists(file_path)) {
        warning("File path already exists, overwritting...")
    }
    if (grepl(".csv", file_path, ignore.case = TRUE)) {
        return(write.csv(content, file_path))
    }
    if (grepl(".geojson", file_path, ignore.case = TRUE) | grepl(".gpkg", file_path, ignore.case = TRUE)) {
        return(sf::st_write(conent, file_path, delete_dns = TRUE))
    }
    if (grepl(".rds", file_path, ignore.case = TRUE) | grepl(".Rds", file_path, ignore.case = TRUE)) {
        return(saveRDS(content, file_path))
    }
    # If got here, throw error, as file format is not supported
    stop("Invalid file extension! Consult the IMPT team to improve this method!")
}
