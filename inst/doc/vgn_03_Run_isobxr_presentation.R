## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")
options(rmarkdown.html_vignette.check_title = FALSE)

## ----echo=FALSE, out.width='100%'---------------------------------------------
knitr::include_graphics('./02_basics_02_runisobxr_wflow.png')

## ----eval = FALSE, message=FALSE, warning=FALSE, paged.print=FALSE------------
#  # To see documentation on run_isobxr function, type in console:
#  ?run_isobxr

## ----echo=FALSE, out.width='100%'---------------------------------------------
knitr::include_graphics('./03_Run_isobxr_01_fill_master.png')

## ----eval=FALSE---------------------------------------------------------------
#  ?run_isobxr

## ----eval=FALSE---------------------------------------------------------------
#  run_isobxr(workdir = "/Users/username/Documents/1_ABC_tutorial",
#             SERIES_ID = "ABC_closed_balanced", # defining the name of the SERIES of run
#             flux_list_name = "Fx1_ABC_closed_bal", # calling the list of fluxes
#             coeff_list_name = "a1", #  calling the list of coefficients
#             t_lim = 2500, # running model for 1000 time units
#             nb_steps = 250, # running model with 100 steps
#             time_units = c("d", "yr")) # plot results in years as time units.

## ----eval=FALSE---------------------------------------------------------------
#  run_isobxr(workdir = "/Users/username/Documents/1_ABC_tutorial",
#             SERIES_ID = "ABC_closed_balanced",
#             flux_list_name = "Fx1_ABC_closed_bal",
#             coeff_list_name = "a1",
#             t_lim = 2500,
#             nb_steps = 250,
#             time_units = c("d", "yr"),
#             to_DIGEST_DIAGRAMS = TRUE, # default is TRUE
#             to_DIGEST_evD_PLOT = TRUE, # default is TRUE
#             to_DIGEST_CSV_XLS = TRUE  # default is FALSE
#             )

## ----echo=TRUE----------------------------------------------------------------
FORCING_SIZE <- 
  data.frame(BOXES_ID = c("BOX_1", "...", "BOX_i", "..."),
             SIZE_INIT = c("updated_size_1", "...", "updated_size_i", "..."))

FORCING_SIZE

## ----echo=TRUE----------------------------------------------------------------
FORCING_SIZE <- 
  data.frame(BOXES_ID = c("C"),
             SIZE_INIT = c(3000))

FORCING_SIZE

## ----echo=TRUE----------------------------------------------------------------
FORCING_DELTA <- 
  data.frame(BOXES_ID = c("BOX_1", "...", "BOX_i", "..."),
             DELTA_INIT = c("updated_delta_1", "...", "updated_delta_i", "..."))

FORCING_DELTA

## ----echo=TRUE----------------------------------------------------------------
FORCING_DELTA <- 
  data.frame(BOXES_ID = c("A"),
             DELTA_INIT = c(-1))

FORCING_DELTA

## ----echo=TRUE----------------------------------------------------------------
FORCING_ALPHA <- 
  data.frame(FROM = c("BOX_i", "..."),
             TO = c("BOX_j", "..."),
             ALPHA = c("new_coeff_value", "..."),
             FROM_TO = c("BOX_i_BOX_j", "..."))

FORCING_ALPHA

## ----echo=TRUE----------------------------------------------------------------
FORCING_ALPHA <- 
  data.frame(FROM = c("A"),
             TO = c("B"),
             ALPHA = c(1.02),
             FROM_TO = c("A_B"))

FORCING_ALPHA

## ----echo=FALSE, out.width='100%'---------------------------------------------
knitr::include_graphics('./03_Run_isobxr_07_Rayleigh.png')

## -----------------------------------------------------------------------------
FORCING_RAYLEIGH <- 
  data.frame(XFROM = c("B"), # Define the B>C flux at numerator
             XTO = c("C"),
             YFROM = c("A"), # Define the A>B flux at denominator
             YTO = c("B"),
             AFROM = c("B"), # Define the resulting fractionation coefficient
             ATO = c("C"),
             ALPHA_0 = c("a0") # Define the value of incremental B>A coefficient
  ) 

FORCING_RAYLEIGH

