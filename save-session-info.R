# save session info

sink("EDcrowding/session_info.txt")
sessionInfo()
sink()

# save installed packages
packages_info <- as.data.table(installed.packages())
write.csv(packages_info, "EDcrowding/installed_packages.csv")
