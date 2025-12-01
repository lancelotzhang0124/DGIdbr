# Local usage example for DGIdbr.

# 1) Install from local source (uncomment if needed)
remotes::install_local(".", force = TRUE)

# 2) Load the package
library(DGIdbr)

# 3) Group mode, keep all drugs (FDA approved + not approved)
DGIdbr(
  mode = "group",
  base_tables = ".",
  group_filename = "group.csv",
  base_out = "dgidb_group_out",
  approve = FALSE
)

# 4) Subtype mode, only FDA approved drugs
DGIdbr(
  mode = "subtype",
  base_tables = ".",
  subtype_filename = "subtype.csv",
  base_out = "dgidb_subtype_out",
  approve = TRUE
)
