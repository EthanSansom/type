.onLoad <- function(lib, pkg) {
  S7::methods_register()
  on_load_core_types()
}
