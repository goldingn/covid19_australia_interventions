pad_cases_matrix <- function(cases, n_dates, which = c("after", "before")) {
  
  which <- match.arg(which)
  
  if (nrow(cases) < n_dates) {
    
    pad <- matrix(0,
                  nrow = n_dates - nrow(cases),
                  ncol = ncol(cases))
    
    if (inherits(cases, "greta_array")) {
      pad <- as_data(pad)
    }
    
    cases <- switch(which,
                    before = rbind(pad, cases),
                    after = rbind(cases, pad))
  }
  
  cases
  
}
