
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
                         Rpath=file.path(R.home("bin"), "Rscript")) {
    session("localsession", libPaths=libPaths, Rpath=Rpath)
}

generateOutput.localsession <- function(session, code, dir,
                                        name, suffix, device, clean) {
    cl <- makePSOCKcluster(1, rscript=session$Rpath)
    f <- function() {
        if (!is.null(session$libPaths)) {
            oldPaths <- .libPaths()
            .libPaths(c(session$libPaths, oldPaths))
        }
        gdiffOutput(code, dir, name, suffix, device, clean)
    }
    clusterCall(cl, f)
    stopCluster(cl)
}

## Networked machine (possibly virtual)
remoteSession <- function(remote, libPaths=NULL, Rpath="Rscript", ...) {
    session("remotesession", libPaths=libPaths, Rpath=Rpath, name=remote,
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

## Running Docker container
dockerSession <- function(libPaths=NULL, Rpath=NULL, ID=NULL, ...) {
    session("dockersession", libPaths=libPaths, Rpath=Rpath, ID=ID)
}

