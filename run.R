Sys.setenv(TAR_PROJECT = "rq1")
targets::tar_make()
targets::tar_destroy(destroy = "all")
