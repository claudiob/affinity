.First.lib <- function(lib, pkg){
    library.dynam("affinity", pkg, lib)
    ehelp <- help(package="affinity")$info[[1]]
    cat(paste(substring(ehelp[4],first=16),"\n",
              "Version ",substring(ehelp[2],first=16),
              " created on ",
               substring(ehelp[3],first=16),".\n", sep=""))
    cat(paste("2008, Claudio Baccigalupo, IIIA-CSIC\n",sep=""))
    cat('Type help(package="affinity") to get started.\n')
}
