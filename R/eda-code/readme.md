This folder contains code to do some simple exploratory data analysis (EDA) on the processed/cleaned data.
The code produces a few tables and figures, which are saved in the appropriate `results` sub-folder.

This code should be run after the /R/processing-code/MADAproject_pt2_ruiz.qmd file. 

The code is contained in an R script that you can run which does all the computations (PT3_edacode.R). 

PT3_eda-v2.qmd is a Quarto file where the code is pulled in from the R script and run. It is currently under development

For now, EDA is performed in separate R files in this folder:

MIR.R
Model.mir.temp.R
Logistic_pos_temp_precip.R

in this phase most of the exploration will take place in R files then called into qmd using rds
