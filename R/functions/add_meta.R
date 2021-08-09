  add_meta <- function(idx, OD_list, meta) {
    meta <- meta[idx, ]
    mutate(OD_list[[idx]],
           start_date = meta$start_date,
           end_date = meta$end_date,
           timestamp = meta$timestamp)
  }
