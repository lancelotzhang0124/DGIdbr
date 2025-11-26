# DGIdbr

Helper functions to query DGIdb for gene sets (case-control or subtype) and write aggregated drug interaction tables.

## Installation

From a local clone:

```r
# install.packages("devtools")
devtools::install_local("DGIdbr")  # or setwd to the cloned folder and use "."
```

From GitHub (after you publish the repo):

```r
# install.packages("remotes")
remotes::install_github("your-org-or-user/DGIdbr")
```

## Usage

```r
library(DGIdbr)

# Case-control (group) mode: input CSV must have columns gene, direction (up/down)
DGIdbr(
  mode = "group",
  base_tables = "path/to/input_dir",
  group_filename = "dep_group_sig.csv",
  base_out = "path/to/output_dir"
)

# Subtype mode: input CSV must have columns gene, direction (up/down), subtype
DGIdbr(
  mode = "subtype",
  base_tables = "path/to/input_dir",
  subtype_filename = "subtype_sig.csv",
  base_out = "path/to/output_dir"
)
```

Environment variable `DGIDB_URL` can override the default DGIdb GraphQL endpoint if needed.
