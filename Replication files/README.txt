
READ ME: Replication file for The Long-Term Effects of Neighborhood Disadvantage on Political Behavior: The “Moving to Opportunity” Experiment  

* note: see the file "overview.docx" for a deeper description of the data creation and analysis processes

This paper relies on datasets that cannot be made publicly available. This archive contains the code used to analyze the original datasets, as well as a limited version of the data and code that can be used to reproduce as many analyses as possible using this limited dataset. Please see the file "overview.docx" for information about how to obtain the full datasets and how the limited dataset was produced.


This archive contains the following files in the root folder:

* Codebook.docx
---a Word document describing the variables in the main dataset used in analysis
* overview.docx
---a Word document describing the data creation and analysis, including detailed descriptions of the code files in this archive


in the Documents folder:
* appendix.pdf
---the primary supplemental information file
* MTO_Tables.pdf
---an aditional supplemental information file showing full tabular versions of all models in the main text and appendix
* preanalysis_plan.pdf
---an analysis plan registered before accessing the data
* pap_departures.pdf
---a document listing compliance with/departures from the preanalysis plan
* IRB_not_human_subjects.txt
---communication from Princeton's IRB determining this project does not constitute human subjects research

in the Merge Files folder:
/Merge Files/Merge Steps:
* 1_cluster_generation.R
---divides the data into clusters to merge the MTO and voter file data
* 2_cluster_generation_b.R
---further divides the data into clusters to merge the MTO and voter file data
* 3_main_code.R
---prepares for and executes the merge
* 4_match_function.R
---a function to merge the MTO and voter file datasets
* 5_reattach_data.R
---exports the merged data
/Merge Files/Merge Steps/output:
--this folder would contain output of the merge process if run
/Merge Files/Merge Steps/temporary_files:
--this folder would contain temporary files created in the merge process if run

/Merge Files/Output Steps:
* 01_create_vote_files_a_to_c.do
---links the merged data with other MTO files
* 02_create_survey_files_d_n.do
---links the merged data with other MTO files
* 03_create_revised_vote_file.do
---processes the MTO data for confidentiality


in the Analysis files folder:
* 1_combine_data.R
---combines the MTO files for analysis
* 2_match_comparison.R
---produces comparisons of matched and full data for Figure 3
* 3_results.R
---produces all analyses in main text and appendix not in other files
* 4_multiple_matches.R
---runs results using multiple matches for appendix section 4
* 5_hte_models.R
---runs models for heterogeneous effects for Figure 4 
* 6_hte_bart.R
---runs models for heterogeneous effects for appendix section 6
* 7_ces_baseline.R
---produces estimates for appendix section 1

/Analysis files/Data:
--this folder would contain raw data files from HUD/NBER

/Analysis files/Figures:
--this folder would contain the figures in the paper/appendix if the analysis files were run

in the Clusters folder:
* create_clusters.R
---processes the full analysis dataset to produce the limited clustered data for sharing
* cluster_results.R
--performs all possible analyses with the clustered data and notes which analyses we cannot perform

* clusters.csv
--this data file contains the clustered data

