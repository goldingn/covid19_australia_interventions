#.libPaths("/home/ryange/covid/gr_covid_lib")
if(sessionInfo()$platform == "x86_64-pc-linux-gnu (64-bit)"){
        if(sessionInfo()$running == "CentOS Linux 7 (Core)"){
                .libPaths("/home/ryange/covid/gr_covid_lib")
        } else if(sessionInfo()$running == "Ubuntu 18.04.4 LTS"){
                .libPaths("/home/unimelb.edu.au/ryange/gr_covid_lib")
        }
}
# 
#  install.packages(
#    pkgs = c(
#      "cli"
#    ),
#    lib = "/home/unimelb.edu.au/ryange/gr_covid_lib",
#    repos = "https://cran.ms.unimelb.edu.au/"
#  )
 # 
 # 
 # devtools::install_github(
 #   repo = "greta-dev/greta.gp"#,
 #   #lib = "/home/ryange/covid/gr_covid_lib/"
 # )