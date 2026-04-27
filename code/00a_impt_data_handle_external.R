library(dplyr)
library(sf)
library(tidyr)


# IMPORTANT! Methods to handle IMPT data  ---------------------------------------------------------
# **This should be the version of the data_handle methods to be used by external collaborators**

# This methods handle the logic to load/store IMPT data. They should be used to handle all files
# They handle internally all the logic to decide where to gather/store the data
# By default, they read/write from ./data folder. Edit the default path_root on the methods below to change this :)

# Always use these functions instead of base R read/write functions, to improve maintainability of file management

impt_read <- function(path, root = NULL, write_to = NULL, ...) { # Use ... for arguments specific for file format reading (e.g. layer for gpkg or delim for csv)
    # If does not start with "/", add it
    if (!startsWith(path, "/")) {
        path <- paste0("/", path)
    }
    # If not forcing base path, set value depending on machine
    path_root <- root
    path_append <- ""
    if (is.null(path_root)) {
        path_root <- "data"
    }
    # Read file and return
    file_path <- paste(path_root, path, path_append, sep = "")
    if (grepl("^http", file_path, ignore.case = TRUE)) { # If http, encode
        file_path <- URLencode(file_path)
    }
    message(sprintf("Downloading file from %s...", file_path))
    if (grepl(".csv", file_path, ignore.case = TRUE)) {
        content <- readr::read_delim(file_path, show_col_types = FALSE, name_repair = make.names, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    if (grepl(".geojson", file_path, ignore.case = TRUE) | grepl(".gpkg", file_path, ignore.case = TRUE)) {
        content <- sf::st_read(file_path, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    if (grepl(".rds", file_path, ignore.case = TRUE) | grepl(".Rds", file_path, ignore.case = TRUE)) {
        if (grepl("^http", file_path, ignore.case = TRUE)) {
            file_local <- file.path(tempdir(), basename(path))
            download.file(file_path, file_local, quiet = TRUE, mode = "wb")
            file_path <- file_local
        }
        content <- readRDS(file_path, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    if (grepl(".xls", file_path, ignore.case = TRUE) | grepl(".xlsx", file_path, ignore.case = TRUE)) {
        if (grepl("^http", file_path, ignore.case = TRUE)) {
            file_local <- file.path(tempdir(), basename(path))
            download.file(file_path, file_local, quiet = TRUE, mode = "wb")
            file_path <- file_local
        }
        content <- readxl::read_excel(file_path, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    # If write_to, download file and return its local path
    if (!is.null(write_to)) {
        destination <- file.path(write_to, basename(path))
        message(sprintf("Storing file at %s...", destination))
        download.file(file_path, destination)
        return(destination)
    }
    # If got here, throw error, as file format is not supported
    stop("Invalid file extension! Consult the IMPT team to improve this method!")
}
impt_write <- function(content, path, root = NULL) { # Use ... for arguments specific for file format reading (e.g. layer for gpkg or delim for csv)
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
        path_root <- "data"
    }
    # Read file and return
    file_path <- paste(path_root, path, path_append, sep = "")
    # Get folder from file_path
    file_folder <- dirname(file_path)
    if (!dir.exists(file_folder)) {
        dir.create(file_folder, recursive = TRUE)
    }
    message(sprintf("Writing file to %s...", file_path))
    # If file exists, warn
    if (file.exists(file_path)) {
        warning("File path already exists, overwritting...")
    }
    if (grepl(".csv", file_path, ignore.case = TRUE)) {
        write.csv(content, file_path, row.names = FALSE)
        return(file_path)
    }
    if (grepl(".geojson", file_path, ignore.case = TRUE) | grepl(".gpkg", file_path, ignore.case = TRUE)) {
        sf::st_write(content, file_path, delete_dsn = TRUE)
        return(file_path)
    }
    if (grepl(".rds", file_path, ignore.case = TRUE) | grepl(".Rds", file_path, ignore.case = TRUE)) {
        saveRDS(content, file_path)
        return(file_path)
    }
    if (grepl(".json", file_path, ignore.case = TRUE)) {
        # Consider content a json string
        if (is.character(content)) {
            writeLines(content, con = file_path)
            return(file_path)
        }
        jsonlite::write_json(content, file_path, auto_unbox = TRUE, pretty = TRUE)
        return(file_path)
    }
    # If got here, throw error, as file format is not supported
    stop("Invalid file extension! Consult the IMPT team to improve this method!")
}
