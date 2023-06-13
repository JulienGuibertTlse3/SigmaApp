# SigmaApp
Tool to build similarity matrices and to compare them.

## Table of contents
* [General info](#general-info)
* [R, Files & Packages](#R-Files-Packages)
* [Explanation](#How-it-works)

## General info
This project is funded by DIGIT-BIO Behind the Counter

![image](https://github.com/JulienGuibertTlse3/SigmaApp/assets/92673300/44108616-9a55-49dd-94ff-d6b3a733b4ed)

And it is linked with the GEroNIMO project.

![image](https://github.com/JulienGuibertTlse3/SigmaApp/assets/92673300/dc3e6113-53b0-422a-b1ab-7fc04c6d9001)

Tool created by Julien GUIBERT under the supervision of Christel Marie-Etancelin and Ingrid David at INRAe Castanet-Tolosan, France.

![image](https://github.com/JulienGuibertTlse3/SigmaApp/assets/92673300/8685a7f2-7bf2-4131-b7ee-8011bc70ccea)

	
## R, Files & Packages
Project is created with:
* R version: 4.2.1

Files available: 
* Shiny_AppS.R
R_func.R
www folder

Packages version: 
* attached packages:          
PLNmodels_1.0.1	         
shinyjs_2.1.0	       
corrplot_0.92	       
zCompositions_1.4.0-1 	      
truncnorm_1.0-8       
NADA_1.6-1.1         
survival_3.3-1        
MASS_7.3-57          
xtable_1.8-4         
DT_0.27             
compositions_2.0-6    
imputeMissings_0.0.3 
imputeTS_3.3          
lubridate_1.9.2       
forcats_1.0.0        
stringr_1.5.0         
dplyr_1.0.10          
purrr_0.3.4          
readr_2.1.4          
tidyr_1.2.1           
tibble_3.1.8         
tidyverse_2.0.0       
vegan_2.6-4           
lattice_0.20-45      
permute_0.9-7         
plotly_4.10.1         
ggplot2_3.4.2        
ape_5.6-2             
shiny_1.7.4 

* loaded via a namespace (and not attached):
colorspace_2.0-3
ellipsis_0.3.2
rsconnect_0.8.29    
glassoFast_1.0       
gridtext_0.1.5       
ggtext_0.1.2        
rstudioapi_0.14      
listenv_0.9.0        
bit64_4.0.5         
fansi_1.0.3         
xml2_1.3.4          
codetools_0.2-18    
splines_4.2.1       
cachem_1.0.6        
robustbase_0.95-0   
jsonlite_1.8.4      
nloptr_2.0.3        
cluster_2.1.3       
compiler_4.2.1      
httr_1.4.5          
Matrix_1.5-1        
fastmap_1.1.0       
lazyeval_0.2.2      
cli_3.4.1           
later_1.3.0         
htmltools_0.5.5     
tools_4.2.1         
igraph_1.3.5        
gtable_0.3.3        
glue_1.6.2          
Rcpp_1.0.9          
jquerylib_0.1.4     
fracdiff_1.5-2      
vctrs_0.5.2         
coro_1.0.3          
urca_1.3-3          
nlme_3.1-159        
lmtest_0.9-40       
timeDate_4022.108   
tensorA_0.36.2      
globals_0.16.2      
ps_1.7.1            
timechange_0.2.0    
mime_0.12           
lifecycle_1.0.3     
future_1.31.0       
DEoptimR_1.0-11     
zoo_1.8-11  
scales_1.2.1
hms_1.1.3   
promises_1.2.0.1    
parallel_4.2.1      
quantmod_0.4.22     
torch_0.9.1         
curl_4.3.2          
memoise_2.0.1       
gridExtra_2.3       
sass_0.4.5          
stringi_1.7.8       
tseries_0.10-54     
randomForest_4.7-1.1
TTR_0.24.3          
rlang_1.1.1         
pkgconfig_2.0.3     
fontawesome_0.5.1   
stinepack_1.4       
htmlwidgets_1.6.2   
bit_4.0.4           
tidyselect_1.2.0    
processx_3.7.0      
parallelly_1.34.0   
magrittr_2.0.3      
R6_2.5.1            
generics_0.1.3      
DBI_1.1.3           
pillar_1.9.0        
withr_2.5.0         
mgcv_1.8-40         
xts_0.13.1          
nnet_7.3-17         
future.apply_1.10.0 
bayesm_3.1-4       
utf8_1.2.2         
tzdb_0.3.0          
grid_4.2.1          
data.table_1.14.8    
callr_3.7.2         
forecast_8.21       
digest_0.6.29        
httpuv_1.6.6        
munsell_0.5.0       
viridisLite_0.4.2   
bslib_0.4.2         
quadprog_1.5-8 
	
## Explanation
To run this project, download the files available and store them in the same folder. (A complete folder to download is also available "Shiny_def")
Afterwards, run the Shiny_AppS.r file to open the tool.

Description of each files:
 * www folder: folder which contains images used in the tool.
 * R_func.R: R file with functions corresponding to each method used to compute the similarity matrices which our main file uses.
 * Shiny_AppS.R: R Script which creates the tool. It is composed of 4 different panels. The first two give information about pre-processing steps and methods available.
The 3rd panel enable the user to choose upload the data he wants to work on, to choose the method and the preproccesing step he wants to use and then to create, store and export the produced matrices.
The last panel enable the user to compare the matrices produced using different method and different type of vizualisation tools.
