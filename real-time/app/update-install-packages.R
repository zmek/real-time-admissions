library(readr)
installed_packages <- data.table(installed = installed.packages())
installed_packages_short <- data.table(read_csv("EDcrowding/real-time/app/installed-packages.csv"))

version = data.table()
for (pkg in installed_packages_short$httr) {
  
  version = bind_rows(version, data.table(package = pkg,
                                          version = installed_packages[installed.Package == pkg, installed.Version]))

}


version[, text := paste0("install_version(\"", package, "\", version = \"", version, "\", repos = \"https://cran.ma.imperial.ac.uk\")")]
 
write.csv(version, "EDcrowding/real-time/app/installed-packages-with-version.csv", sep = "'", append = FALSE)
  
  

