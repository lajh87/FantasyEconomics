policies <- readxl::read_xlsx("data-raw/policies.xlsx",1)
scores <- readxl::read_xlsx("data-raw/policies.xlsx",2)

usethis::use_data(policies, overwrite = TRUE)
usethis::use_data(scores, overwrite = TRUE)
