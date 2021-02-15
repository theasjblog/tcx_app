source('dependencies.R')
saveAllData <- function(branchName = 'master'){
  # remove old version of R files ---------------------------------
  listFiles <- list.files('./auxFunctions')
  for (i in listFiles){
    if (i != 'refreshApp.R'){
      file.remove(paste0('./auxFunctions/', i))
    }
  }
  
  # get newest version of covid19Visualizer ---------------------------------
  # ideally we deploy installing the package,
  # but shinyapps.io fails to do that
  download.file(url = paste0("https://github.com/theasjblog/tcx_package/archive/",branchName,".zip")
                , destfile = "tcx_package.zip")
  unzip(zipfile = "tcx_package.zip")
  
  listFiles <- list.files(paste0('./tcx_package-',branchName,'/R'))
  for (i in listFiles){
    file.copy(from = paste0('./tcx_package-', branchName, '/R/', i), 
              to = paste0('./auxFunctions/', i), 
              overwrite = TRUE)
    source(paste0('./auxFunctions/', i))
  }
}
