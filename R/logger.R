#' @name Logger functions
#' @rdname logger
#' @aliases logger
#' 
#' @title Logger functions
#' 
#' @description Functions for creating a logger object and manipulating the pertaining log file.
#' 
#' @param path must exits.
#' @param suffix .
#' @param prefix .
#' @param file A character string with the path to a log file.
#' @param type .
#' @param msg .
#'
#' @details M = MESSAGE etc.
#'
#' @return 
#'
#' @examples
#' \dontrun{
#' logger.path <- "...path to log directory..."
#' logger.suffix <- format(Sys.Date(), "%Y%m%d")
#' # Create logger object
#' logger <- logger.create(logger.path, logger.suffix)
#' # Log file
#' logger()$file
#' # Write to log
#' logger()$write("INFO", "This is a test message")
#' logger()$write("SUCCESS", "Success")
#' logger()$write("WARNING", "This is a test warning")
#' logger()$write("ERROR", "This is a test error")
#' logger()$write("FATAL", "There was a fatal error")
#' # Read log
#' logger()$read()
#' # Truncate log
#' logger()$truncate()
#' logger()$read()
#' }
#'
NULL


#' @rdname logger
#' @export
logger.write <- function (file, type, msg) {
  type <- match.type(type)
  msg <- paste(Sys.time(), type, msg, sep = "|")
  con <- file(file)
  open(con, open = "a")
  writeLines(msg, con)
  close(con)
#  if (print) {
#    writeLines(strwrap(msg, exdent = 2))
#  }
}

#' @rdname logger
#' @export
logger.read <- function (file) cat(paste(readLines(file), collapse = "\n"), "\n")

#' @rdname logger
#' @export
logger.truncate <- function (file) {
  con <- file(file)
  open(con, open = "w+b")
  close(con)
}

#' @rdname logger
#' @export
logger.create <- function (path, suffix, prefix = "log_") {
  logfile <- file.path(path, paste0(prefix, suffix))
  if(!file.exists(logfile)) {
    file.create(logfile)
    logger.write(logfile, "INFO", "Created log file")
  }
  assign("file", logfile, envir = logger.env)
  logger <- function () {
    list("file" = logfile,
         "write" = function (type, msg) {
           logger.write(get("file", envir = logger.env), type, msg)
         },
         "read" = function () logger.read(get("file", envir = logger.env)),
         "truncate" = function () {
           logger.truncate(get("file", envir = logger.env))
         })
  }
  class(logger) <- append(class(logger), "logger")
  return(logger)
}


match.type <- function (x) {
  types <- c("INFO", "SUCCESS", "WARNING", "ERROR", "FATAL")
  type <- types[pmatch(toupper(x), types)]
  if (is.na(type)) type <- x
  return (type)
}




## Arguments to be passed to `logger.create` 
#logger.path = "/wales/R/tmp/logs"
#logger.suffix = "20200601"


## Create logger object
#logger <- logger.create(logger.path, logger.suffix)
## Loger file path
#logger()$file
## Write to log
#logger()$write("INFO", "Test1")
#logger()$write("INFO", "Test2")
## Read log
#logger()$read()
## Truncate log
#logger()$truncate()
#logger()$read()
