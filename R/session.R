
## Define "gdiff.session" objects

## FOR NOW, all sessions must see (mount) the same file system

session <- function(class, ...) {
    h <- list(...)
    class(h) <- c(class, "gdiff.session")
    h
}

## The current R session
currentSession <- function(libPaths=NULL) {
    session("currentsession", libPaths=libPaths)
}

generateOutput.currentsession <- function(session, code, dir,
                                          name, suffix, device, clean) {
    oldPaths <- .libPaths()
    if (!is.null(session$libPaths)) {
        .libPaths(c(session$libPaths, oldPaths))
    }
    gdiffOutput(code, dir, name, suffix, device, clean)
    if (!is.null(session$libPaths)) {
        .libPaths(oldPaths)
    }
}

## R session on local machine
localSession <- function(libPaths=NULL,
                         Rpath=file.path(R.home("bin"), "Rscript"),
                         ...) {
    session("localsession", libPaths=libPaths, Rpath=Rpath,
            clusterArgs=list(...))
}

generateOutput.localsession <- function(session, code, dir,
                                        name, suffix, device, clean) {
    ## Avoid worker session getting library paths from master session
    libVars <- Sys.getenv(c("R_LIBS", "R_LIBS_SITE", "R_LIBS_USER"))
    Sys.setenv(R_LIBS="", R_LIBS_SITE="", R_LIBS_USER="")
    on.exit(do.call(Sys.setenv, as.list(libVars)))
    
    cl <- do.call(makePSOCKcluster,
                  c(list(1, rscript=session$Rpath),
                    session$clusterArgs))
    on.exit(stopCluster(cl))
            
    f <- function() {
        if (!require("gdiff")) {
            install.packages("gdiff")
        }
        if (!is.null(session$libPaths)) {
            oldPaths <- .libPaths()
            .libPaths(c(session$libPaths, oldPaths))
        }
        gdiffOutput(code, dir, name, suffix, device, clean)
    }
    clusterCall(cl, f)
}

## Networked machine (possibly virtual)
remoteSession <- function(remote, ...) {
    UseMethod("remoteSession")
}

remoteSession.character <- function(remote, libPaths=NULL, Rpath="Rscript",
                                    ...) {
    session("remotesession", libPaths=libPaths, Rpath=Rpath, remote=remote,
            clusterArgs=list(...))
}

generateOutput.remotesession <- function(session, code, dir,
                                         name, suffix, device, clean) {
    ## Use ssh::ssh_connect() and ssh::ssh_exec_wait() ?
    ## Generate output on remote session
    cl <- do.call(makePSOCKcluster,
                  c(list(session$name, rscript=session$Rpath),
                    session$clusterArgs))
    f <- function() {
        if (!require("gdiff")) {
            install.packages("gdiff")
        }
        outputDir <- tempfile("gdiffOutput")
        dir.create(outputDir)
        if (!is.null(session$libPaths)) {
            oldPaths <- .libPaths()
            .libPaths(c(session$libPaths, oldPaths))
        }
        gdiffOutput(code, outputDir, name, suffix, device, clean)
        outputDir
    }
    outputDir <- clusterCall(cl, f)[[1]]
    ## Harvest output from remote session
    if ("user" %in% names(session$clusterArgs)) {
        host <- paste0(session$clusterArgs$user, "@", session$name)
    } else {
        host <- session$name
    }
    createDir(dir, clean)
    con <- ssh_connect(host)
    scp_download(con, file.path(outputDir, "*"), dir, verbose=FALSE)
    ssh_disconnect(con)
    stopCluster(cl)    
}

remoteSession.cluster <- function(remote, libPaths=NULL, user=NULL, ...) {
    session("clustersession", libPaths=libPaths, user=user, remote=remote)
}

generateOutput.clustersession <- function(session, code, dir,
                                          name, suffix, device, clean) {
    f <- function() {
        if (!require("gdiff")) {
            install.packages("gdiff")
        }
        outputDir <- tempfile("gdiffOutput")
        dir.create(outputDir)
        if (!is.null(session$libPaths)) {
            oldPaths <- .libPaths()
            .libPaths(c(session$libPaths, oldPaths))
        }
        gdiffOutput(code, outputDir, name, suffix, device, clean)
        outputDir
    }
    outputDir <- clusterCall(cl, f)[[1]]
    ## Harvest output from remote session
    if (is.null(session$user)) {
        host <- session$remote[[1]]$host
    } else {
        host <- paste0(session$user, "@", session$remote[[1]]$host)
    }
    createDir(dir, clean)
    con <- ssh_connect(host)
    scp_download(con, file.path(outputDir, "*"), dir, verbose=FALSE)
    ssh_disconnect(con)
}

## Running Docker container
dockerSession <- function(image, libPaths=NULL, Rpath="Rscript", ...) {
    session("dockersession", libPaths=libPaths, Rpath=Rpath, image=image)
}

generateOutput.dockersession <- function(session, code, dir,
                                         name, suffix, device, clean) {
    ## Create/clean output directory first
    ## (Docker container will just generate output in there)
    createDir(dir, clean)
    docker <- docker_client()
    ## Create container 
    container <- docker$container$create(session$image,
                                         ## Keep container open
                                         "/bin/bash", tty=TRUE,
                                         ## Mount local output directory
                                         volumes=paste0(normalizePath(dir),
                                                        ":/work"),
                                         working_dir="/work")
    container$start()
    ## Run R in container to generate output
    paths <- session$libPaths
    f <- function() {
        if (!require("gdiff")) {
            install.packages("gdiff")
        }
        if (!is.null(paths)) {
            oldPaths <- .libPaths()
            .libPaths(c(paths, oldPaths))
        }
        gdiff::gdiffOutput(code, ".", name, suffix, device, clean=FALSE)
    }
    environment(f) <- globalenv()
    funFile <- file.path(dir, "gdiff.rda")
    save("f", "paths", "code", "name", "suffix", "device", "clean",
         file=funFile)
    cmd <- c(session$Rpath, "-e", "load(\"gdiff.rda\"); f()")
    container$exec(cmd)
    container$stop()
    container$remove()
    ## Clean up
    unlink(funFile)
}
