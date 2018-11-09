#!/usr/bin/Rscript

for (lib in c("shiny", "markdown", "ggplot2", "lme4")){
    if(! lib  %in% rownames(installed.packages())) {
        print(paste("Installing", lib) )
        install.packages(lib, repos='http://cran.us.r-project.org')
    }
}
