#' module.copy
#'
#' The module copy function moves lesson-plan files from an internal package directory to a user-specified directory.
#'
#' @param mod module to be copied, specified in quotes 
#' @param to path to which the module should be copied, should end with a folder name
#' @return NULL only moving files around
#' @examples 
#'    module.copy(mod="permTestExample", to="~/test")
#' 
#' @export
module.copy <- function(mod, to) {
        
        ## check to see if module already exists
        full.path <- paste(to, mod, sep="")
        if(file.exists(full.path)) {
                check.path <- paste("Ok to overwrite existing files/folders at", full.path, "?")
                if(yesno(check.path)){
                        message("Please select a different module destination and try again.")
                        return(invisible())
                }
        } else if(!file.exists(to))
                dir.create(to)
        ## locate the module in the system file structure
        mod.loc.sys <- system.file(package="teachr", mod)
        ## copy the module to the desired location
        file.copy(from=mod.loc.sys, to=to, recursive=TRUE)
        message(paste("module", mod, "copied successfully"))
        return(invisible())
}

yesno <- function(question) {
        ## adapted from release.R in devtools
        yeses <- c("Yes", "Definitely", "For sure", "Yup", "Yeah")
        nos <- c("No way", "Not yet", "No", "Nope")
        
        cat(question)
        qs <- c(sample(yeses, 1), sample(nos, 2))
        rand <- sample(length(qs))
        
        menu(qs[rand]) != which(rand == 1)
}