
## Define "gdiffSession" objects
gdiffSession <- function(class, ...) {
    h <- list(...)
    class(h) <- c(class, "gdiffSession")
    h
}

## The current R session
currentSession <- function(libPaths=NULL) {
    gdiffSession("gdiffCurrentSession", libPaths=libPaths)
}

generateOutput.gdiffCurrentSession <- function(session, codeFun,
                                               dir, device, clean, ncpu) {
    oldPaths <- .libPaths()
    if (!is.null(session$libPaths)) {
        .libPaths(c(session$libPaths, oldPaths))
        on.exit(.libPaths(oldPaths))
    }
    gdiffGenerateOutput(codeFun, dir, device, clean, ncpu)
}

## R session on local machine
localSession <- function(libPaths=NULL,
                         Rpath=file.path(R.home("bin"), "Rscript"),
                         ...) {
    gdiffSession("gdiffLocalSession", libPaths=libPaths, Rpath=Rpath,
                 clusterArgs=list(...))
}

generateOutput.gdiffLocalSession <- function(session, codeFun,
                                             dir, device, clean, ncpu) {
    ## Avoid worker session getting library paths from master session
    libVars <- Sys.getenv(c("R_LIBS", "R_LIBS_SITE", "R_LIBS_USER"))
    Sys.setenv(R_LIBS="", R_LIBS_SITE="", R_LIBS_USER="")
    on.exit(do.call(Sys.setenv, as.list(libVars)))
    
    cl <- do.call(makePSOCKcluster,
                  c(list(1, rscript=session$Rpath),
                    session$clusterArgs))
    on.exit(stopCluster(cl))

    oldPaths <- .libPaths()
    if (!is.null(session$libPaths)) {
        newPaths <- c(session$libPaths, oldPaths)
    } else {
        newPaths <- oldPaths
    }
    
    ## Appears to need to be a separate call (WTF?)
    f <- function() {
        .libPaths(newPaths)
        require(gdiff)
    }
    
    clusterCall(cl, f)
    
    f <- function() {
        gdiffGenerateOutput(codeFun, dir, device, clean, ncpu)
    }
    
    clusterCall(cl, f)
}

## Networked machine (possibly virtual)
remoteSession <- function(remote, ...) {
    if (!requireNamespace("ssh", quietly = TRUE)) {
        stop("The 'ssh' package must be installed")
    }
    UseMethod("remoteSession")
}

remoteSession.character <- function(remote, libPaths=NULL, Rpath="Rscript",
                                    ...) {
    gdiffSession("gdiffRemoteSession",
                 libPaths=libPaths, Rpath=Rpath, remote=remote,
                 clusterArgs=list(...))
}

generateOutput.gdiffRemoteSession <- function(session, codeFun,
                                              dir, device, clean, ncpu) {
    ## Use ssh::ssh_connect() and ssh::ssh_exec_wait() ?
    ## Generate output on remote session
    cl <- do.call(makePSOCKcluster,
                  c(list(session$remote, rscript=session$Rpath),
                    session$clusterArgs))

    f <- function() {
        outputDir <- tempfile("gdiffOutput")
        dir.create(outputDir)
        if (!is.null(session$libPaths)) {
            oldPaths <- .libPaths()
            .libPaths(c(session$libPaths, oldPaths))
        }
        require(gdiff)
        gdiffGenerateOutput(codeFun, outputDir, device, clean, ncpu)
        outputDir
    }

    outputDir <- clusterCall(cl, f)[[1]]

    ## Harvest output from remote session
    if ("user" %in% names(session$clusterArgs)) {
        host <- paste0(session$clusterArgs$user, "@", session$remote)
    } else {
        host <- session$remote
    }
    createDir(dir, clean)
    con <- ssh::ssh_connect(host)
    ## For R CMD check
    outputFiles <- ""
    con2 <- textConnection("outputFiles", "w", local=TRUE)
    ssh::ssh_exec_wait(con, paste0("ls -A ", outputDir), std_out=con2)
    close(con2)
    lapply(outputFiles,
           function(x) {
               ssh::scp_download(con, file.path(outputDir, x), dir,
                                 verbose=FALSE)
           })
    ssh::ssh_disconnect(con)
    stopCluster(cl)    
}

remoteSession.cluster <- function(remote, libPaths=NULL, user=NULL, ...) {
    gdiffSession("gdiffClusterSession",
                 libPaths=libPaths, user=user, remote=remote)
}

generateClusterOutput <- function(session, dir, clean, f) {
}

generateOutput.gdiffClusterSession <- function(session, codeFun,
                                               dir, device, clean, ncpu) {
    f <- function() {
        outputDir <- tempfile("gdiffOutput")
        dir.create(outputDir)
        if (!is.null(session$libPaths)) {
            oldPaths <- .libPaths()
            .libPaths(c(session$libPaths, oldPaths))
        }
        require(gdiff)
        gdiffGenerateOutput(codeFun, outputDir, device, clean, ncpu)
        outputDir
    }

    outputDir <- clusterCall(session$remote, f)[[1]]

    ## Harvest output from remote session
    if (is.null(session$user)) {
        host <- session$remote[[1]]$host
    } else {
        host <- paste0(session$user, "@", session$remote[[1]]$host)
    }
    createDir(dir, clean)
    con <- ssh::ssh_connect(host)
    ## For R CMD check
    outputFiles <- ""
    con2 <- textConnection("outputFiles", "w", local=TRUE)
    ssh::ssh_exec_wait(con, paste0("ls -A ", outputDir), std_out=con2)
    close(con2)
    lapply(outputFiles,
           function(x) {
               ssh::scp_download(con, file.path(outputDir, x), dir,
                                 verbose=FALSE)
           })
    ssh::ssh_disconnect(con)
}

## Running Docker container
dockerSession <- function(image, volumes=NULL, env=NULL, network="bridge",
                          libPaths=NULL, Rpath="Rscript", ...) {
    if (!requireNamespace("stevedore", quietly = TRUE)) {
        stop("The 'stevedore' package must be installed")
    }
    gdiffSession("gdiffDockerSession",
                 libPaths=libPaths, Rpath=Rpath,
                 image=image, volumes=volumes, network=network)
}

generateOutput.gdiffDockerSession <- function(session, codeFun,
                                              dir, device, clean, ncpu) {

    ## 'codeFun' can be NULL, in which case do nothing
    if (is.null(codeFun)) return()
    
    ## Create/clean output directory first
    ## (Docker container will just generate output in there)
    createDir(dir, clean)

    docker <- stevedore::docker_client()
    ## Create container
    container <- docker$container$create(session$image,
                                         ## Keep container open
                                         "/bin/bash", tty=TRUE,
                                         ## Mount local output directory
                                         volumes=c(session$volumes,
                                                   paste0(normalizePath(dir),
                                                          ":/home/gdiff")),
                                         ## Use network as host
                                         network=session$network,
                                         ## Set environment variables
                                         env=session$env,
                                         working_dir="/home/gdiff")
    container$start()
    
    paths <- session$libPaths
    f <- function() {
        if (!is.null(paths)) {
            oldPaths <- .libPaths()
            .libPaths(c(paths, oldPaths))
        }
        require(gdiff)
        gdiffGenerateOutput(codeFun, ".", device, clean=FALSE, ncpu)
    }
    environment(f) <- globalenv()
    funFile <- file.path(dir, "gdiff.rda")
    save("f", "paths", "codeFun", "device", "clean", "ncpu",
         file=funFile)
    
    ## Run R in container to generate output
    cmd <- c(session$Rpath, "-e", "load(\"gdiff.rda\"); f()")
    container$exec(cmd)
    container$stop()
    container$remove()
    
    ## Clean up
    unlink(funFile)
}

