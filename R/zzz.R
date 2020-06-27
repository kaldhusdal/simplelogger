.onLoad <- function (lib, pkg){
  # Create environment for session specific logger settings/objects
  # ls(logger:::logger.env)
  assign("logger.env", new.env(), envir = parent.env(environment()))
}
