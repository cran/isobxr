## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----echo=FALSE, out.width='75%'----------------------------------------------
knitr::include_graphics('./07_std_01_RUN_LIST.png')

## ----echo=FALSE, out.width='50%'----------------------------------------------
knitr::include_graphics('./07_std_02_FORCING_SIZE.png')

## ----echo=FALSE, out.width='50%'----------------------------------------------
knitr::include_graphics('./07_std_03_FORCING_DELTA.png')

## ----echo=FALSE, out.width='50%'----------------------------------------------
knitr::include_graphics('./07_std_04_ALPHA.png')

## ----echo=FALSE, out.width='75%'----------------------------------------------
knitr::include_graphics('./07_std_05_FORCING_RAYLEIGH.png')

## ----echo=FALSE---------------------------------------------------------------
# explore a series of flux lists as defined in isobxr master file
"EXPLO_n_FLUX_MATRICES" 

# explore a series of coeff lists as defined in isobxr master file
"EXPLO_n_ALPHA_MATRICES" 

# explore a vector of sizes for a given box 
"EXPLO_1_SIZE" 

# explore a vector of delta values for a given box 
"EXPLO_1_DELTA" 

# explore a vector of alpha (coeff) values for a given flux
"EXPLO_1_ALPHA" 

# explore a vector of incremental alpha values for a Rayleigh distillation model
"EXPLO_1_RAYLEIGH_ALPHA" 


## ----echo=TRUE, results = "hide"----------------------------------------------
data.frame(VALUES_1 = c("flux_list_1",  # RUN #1 vector of n strings of chars.
                        "...", 
                        "flux_list_i", 
                        "...", 
                        "flux_list_n"),  
           VALUES_2 = c("flux_list_1",  # RUN #2 vector of n strings of chars.
                        "...", 
                        "flux_list_i", 
                        "...", 
                        "flux_list_n"),  
           EXPLO_TYPES = "EXPLO_n_FLUX_MATRICES") # stricly leave as such


## ----echo=TRUE, results = "hide"----------------------------------------------
data.frame(VALUES_1 = c("coeff_list_1", # RUN #1 vector of n strings of chars.
                        "...", 
                        "coeff_list_i", 
                        "...", 
                        "coeff_list_n"), 
           VALUES_2 = c("coeff_list_1", # RUN #2 vector of n strings of chars.
                        "...", 
                        "coeff_list_i", 
                        "...", 
                        "coeff_list_n"), 
           EXPLO_TYPES = "EXPLO_n_ALPHA_MATRICES") # stricly leave as such



## ----echo=TRUE, results = "hide"----------------------------------------------
data.frame(BOXES_ID = "BOX_i", # 1 string of char
           SIZE_MIN = "min_sweep_value", # 1 numerical value
           SIZE_MAX = "max_sweep_value", # 1 numerical value
           SIZE_STEPS = "sweep_steps", # 1 numerical value
           EXPLO_TYPES = "EXPLO_1_SIZE") # stricly leave as such



## ----echo=TRUE, results = "hide"----------------------------------------------
data.frame(BOXES_ID = "BOX_i", # 1 string of char
           DELTA_MIN = "min_sweep_value", # 1 numerical value
           DELTA_MAX = "max_sweep_value", # 1 numerical value
           DELTA_STEPS = "sweep_steps", # 1 numerical value
           EXPLO_TYPES = "EXPLO_1_DELTA") # stricly leave as such



## ----echo=TRUE, results = "hide"----------------------------------------------
data.frame(FROM = "BOX_i", # 1 string of char
           TO = "BOX_j", # 1 string of char
           ALPHA_MIN = "min_sweep_value", # 1 numerical value
           ALPHA_MAX = "max_sweep_value", # 1 numerical value
           ALPHA_STEPS = "sweep_steps", # 1 numerical value
           EXPLO_TYPES = "EXPLO_1_ALPHA") # stricly leave as such



## ----echo=TRUE, results = "hide"----------------------------------------------
data.frame(XFROM = "Box_B", # B>C flux at numerator (strings of char)
           XTO = "Box_C",
           YFROM = "Box_A", # A>B flux at denominator (strings of char)
           YTO = "Box_B",
           AFROM = "Box_B", # resulting fract. coefficient (strings of char)
           ATO = "Box_C",
           ALPHA_0_MIN = "min_sweep_value", # value of incremental B>A coeff. (num.)
           ALPHA_0_MAX = "max_sweep_value", 
           ALPHA_0_STEPS = "sweep_steps",
           EXPLO_TYPES = "EXPLO_1_RAYLEIGH_ALPHA") # stricly leave as such



## ----eval=FALSE, include=TRUE-------------------------------------------------
#  sweep_dyn(workdir = "/Users/username/Documents/1_ABC_tutorial",
#            # name of the series ID
#            SERIES_ID = "ABC_change_source_sweep_dyn_demo1",
#            # in/out time units for pdf plots
#            time_units = c("d", "yr"),
#            # name of sweep dyn master file
#            EXPLO_MASTER = "0_SWEEP_DYN_MASTER.xlsx",
#            # make alpha A to C vary between 0.9988 and 1
#            EXPLO_AXIS_1 = data.frame(FROM = c("A"),
#                                      TO = c("C"),
#                                      ALPHA_MIN = 0.9988,
#                                      ALPHA_MAX = 1,
#                                      ALPHA_STEPS = 0.0004,
#                                      EXPLO_TYPES = "EXPLO_1_ALPHA"),
#             # make size of B vary between 500 and 4000
#            EXPLO_AXIS_2 =  data.frame(BOXES_ID = c("B"),
#                                       SIZE_MIN = 500,
#                                       SIZE_MAX = 4000,
#                                       SIZE_STEPS = 500,
#                                       EXPLO_TYPES = "EXPLO_1_SIZE"),
#            to_DYN_DIGEST_CSVs = TRUE)

