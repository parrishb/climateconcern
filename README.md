# climateconcern
Replication package for Global geographic variation in climate concern at national and sub-national scales. 

The replication archive includes all data files used as inputs and created as outputs from a series of scripts that clean, format, and analyze the outputs from our climate concern estimation model. Inputs and outputs from all scripts are included, such that users can start at any point in the sequence of scripts. We recommend downloading all files, using "original format," which will download a zip file that preserves the folder structure encoded in the scripts for reading in data and exporting data objects, figures, etc. 

Users have a few options for replicating the analyses shown in the paper. One option is to start with the outputs from our climate concern estimation model. For this option, users should start with the script called "1_clean_output_globalmrp_regionmodel.R" and proceed through the other numbered scripts. 

Users can also skip cleaning the outputs from the climate concern estimation model and start with script "2_analyze_output_globalmrp_regionmodel.R," which takes as its inputs the cleaned and formatted outputs from the first script and creates the validation figures in the paper. Alternatively, scripts 3 and 4 contain the code needed to produce all of the descriptive figures included in the main text and supplementary information. All of the data needed to run these scripts are included in the replication archive; users need not run the scripts 1 and 2 in order to run scripts 3 and 4. 

Users who wish to replicate the climate concern estimation model should start with the script called "run_model.R." This model pulls prepared inputs from the folder called "data." The model takes several days to run and is best run on a remote cluster. After running the estimation model, users can clean and analyze the data using the numbered scripts. 

To fully replicate the steps we took to estimate the model and analyze our results, the scripts should be run in the order shown here: 

1) run_model.R calls the Stan model (climateconcern_model.stan) to estimate climate concern. The inputs for this model are contained in the folder called "data" and are the outputs from rstan_prepdata_region.R.

2) 1_clean_output_globalmrp_regionmodel.R takes in the output from the climate concern estimation model and cleans up the model outputs for the analyses shown in the paper. Some of the validation results will not replicate perfectly because the terms of our data use agreements do not allow us to share all of the micro-data that serve as inputs to the model. 

3) 2_analyze_output_globalmrp_regionmodel.R runs the validation exercises presented in the Online Methods and Supplementary Information. 

4) 3_descriptive_plots_globalmrp_regionmodel.R creates Figures 1-4 and Figure 6 of the main paper, along with the maps and other descriptive figures included in the SI. 

5) 4_subnational descriptives.R produces Figure 5 and Figure S19, which visualize subnational variation 

An additional script is included, which is called rstan_prepdata_region.R. This script prepares the data for input into the climate concern model. The outputs from this script are contained in the folder called "data." We have included this script for transparency, but we are not authorized to share the individual-level data for some of the surveys included in our model. We have uploaded a modified version of the individual-level data (the input into rstan_prepdata_region.R), and this publicly shared version does not include the datasets we are not authorized to share. This means that users will not be able to replicate our results without obtaining for themselves these datasets. A separate text file will be uploaded with full information about these surveys and how users can obtain access to them and prepare them for inclusion in the model. 