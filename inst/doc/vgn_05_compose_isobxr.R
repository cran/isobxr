## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----echo=FALSE, out.width='100%'---------------------------------------------
knitr::include_graphics('./05_cps_01_principle.png')

## ----echo=FALSE, out.width='100%'---------------------------------------------
knitr::include_graphics('./05_cps_04_filling_master.png')

## ----eval=FALSE---------------------------------------------------------------
#  ?compose_isobxr

## ----eval=FALSE---------------------------------------------------------------
#  compose_isobxr(workdir = "/Users/username/Documents/1_ABC_tutorial",
#                 SERIES_ID = "ABC_change_balance", # series ID of composite family
#                 time_units = c("d", "yr"), # plot results in years as time units
#                 COMPO_MASTER = "0_CPS_MASTER_changing_balance.xlsx") # composite master

## ----eval=FALSE---------------------------------------------------------------
#  compose_isobxr(workdir = "/Users/username/Documents/1_ABC_tutorial",
#                 SERIES_ID = "ABC_change_balance", # series ID of composite family
#                 time_units = c("d", "yr"), # plot results in years as time units
#                 COMPO_MASTER = "0_CPS_MASTER_changing_balance.xlsx", # composite master
#                 plot_HIDE_BOXES_delta = c("SINK"), # Hide SINK in evD plots
#                 plot_HIDE_BOXES_size = c("SINK", "SOURCE"), # Hide SINK and SOURCE in evS plots
#                 EACH_RUN_DIGEST = TRUE,
#                 to_CPS_DIGEST_CSVs = TRUE)

