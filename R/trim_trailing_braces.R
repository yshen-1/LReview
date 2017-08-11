trim_trailing_braces <- function(x) {
  gsub("^\\{+|\\}+$", "", x)
}
