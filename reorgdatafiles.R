library(fs)
library(dplyr)

# 1. Assume icos_manifest is already in memory

# 2. New base output directory
base_out <- file.path("data", "FLUXNET", "ICOS")

# 3. Decide each file’s destination
icos_manifest2 <- icos_manifest_new %>%
  mutate(
    dest_dir = if_else(
      is.na(time_integral) | is.na(dataset),
      base_out,
      file.path(base_out, paste0(time_integral, "_", dataset))
    )
  )

# 4. Create all destination directories using base R (guaranteed to recurse)
for (d in unique(icos_manifest2$dest_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# 5. Move each file, but only if it isn’t already there
icos_manifest2 %>%
  rowwise() %>%
  do({
    src  <- .$path
    dest <- file.path(.$dest_dir, basename(src))
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    
    if (src != dest) {
      # always fetch the source mtime
      src_mtime <- file.info(src)$mtime
      
      # decide if we should move
      should_move <- if (file.exists(dest)) {
        # dest exists → only move if source is strictly newer
        dest_mtime <- file.info(dest)$mtime
        src_mtime > dest_mtime
      } else {
        # dest does not exist → definitely move
        TRUE
      }
      
      if (should_move) {
        # copy & overwrite (or initial move), then delete original
        file.copy(src, dest, overwrite = TRUE)
        file.remove(src)
      }
    }
    
    tibble()  # dummy return
  })

cat("All ICOS downloads are now under", base_out, "\n")



#AMF files 
library(fs)
library(dplyr)

# 1. Assume AMF_manifest_new is already in memory:
#    AMF_manifest_new <- discover_AMF_files("data")

# 2. Define base output directory for AMF
base_out_amf <- file.path("data", "FLUXNET", "AMF")

# 3. Compute each file’s destination folder
amf_manifest2 <- AMF_manifest_new %>%
  mutate(
    dest_dir = if_else(
      is.na(time_integral) | is.na(dataset),
      base_out_amf,
      file.path(base_out_amf, paste0(time_integral, "_", dataset))
    )
  )

# 4. Create all needed directories
for (d in unique(amf_manifest2$dest_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# 5. Move each file into its folder (skip if already there)
amf_manifest2 %>%
  rowwise() %>%
  do({
    src  <- .$path
    dest <- file.path(.$dest_dir, basename(src))
    # ensure parent dir exists
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    # move only if not already in place
    # get file info
    src_info  <- file.info(src)
    dest_info <- file.info(dest)
    
    # decide whether to move/overwrite
    if (src != dest) {
      if (!file.exists(dest) || src_info$mtime > dest_info$mtime) {
        # copy over, overwriting if it’s already there
        file.copy(src, dest, overwrite = TRUE)
        # then delete the original
        file.remove(src)
      }
      # otherwise do nothing (dest is up‐to‐date or newer)
    }
    
    tibble()  # dummy return to satisfy rowwise
  })

cat("All AMF FLUXNET files have been moved under", base_out_amf, "\n")

