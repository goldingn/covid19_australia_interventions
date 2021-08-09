url_exists <- function(address) {  
  tryCatch(
    {  
      con <- url(address)  
      a  <- capture.output(suppressWarnings(readLines(con)))  
      close(con)  
      TRUE;  
    },  
    error = function(err) {  
      occur <- grep("cannot open the connection", capture.output(err));  
      if(length(occur) > 0) FALSE;  
    }  
  )  
}
