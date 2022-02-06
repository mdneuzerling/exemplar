template_header <- function(suffix = "data") {
  c(
    paste0("validate_", suffix, " <- function(data) {"),
    "stopifnot(exprs = {"
  )
}

append_to_template <- function(template, x) {
  c(template, x)
}

append_assertion <- function(template, assertion) {
  append_to_template(template, assertion)
}

add_new_line_to_template <- function(template) {
  append_to_template(template, "")
}

# close, in order:
# 1. the expression block within stopifnot }
# 2. the arguments to stopifnot )
# 3. the overall validate function }
template_footer <- function() c("}", ")", "}")
