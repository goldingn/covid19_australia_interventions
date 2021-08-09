# copy over all new NNDSS linelist files from the shared drive to an unsynced local store
sync_nndss <- function(mount_dir = "~/Mounts/nndss", storage_dir = "~/not_synced/nndss") {
  
  # mount the drive
  system("mount_nndss", ignore.stderr = TRUE)
  Sys.sleep(5)
  
  from_files <- list.files(mount_dir, full.names = TRUE)
  existing_files <- list.files(storage_dir)
  new <- !(basename(from_files) %in% existing_files)
  files_to_read <- from_files[new]
  for (new_file in files_to_read) {
    file.copy(new_file, file.path(storage_dir, basename(new_file)), )
  }
}
