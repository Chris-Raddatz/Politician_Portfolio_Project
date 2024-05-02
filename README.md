# Team-61 README
Team 61's Project Repository for MGT 6203 - Spring 2024.

Team Members **Adam Johnson, Nick Loeffelholz, Christopher Raddatz, Yatri Patel**

All testing analysis can be run through the RMD File "Final_Walkthrough.Rmd", this file contains the necessary R packages to be used in the analysis. 

Our repository's main compartmentalization is this:
1) **Code** : Contains the main analysis files
- portfolio_analysis_overall.R : General portfolio analysis versus the S&P 500.
- portfolio_analysis_age.R : Portfolio analysis with politicians filtered via age performance against the S&P 500.
- portfolio_analysis_party.R : Portfolio analysis with politicians filtered via party affiliation performance against the S&P 500.
2) **Data** : Contains R code for fetching and cleaning data we used throughout the project as well as CSV files
- All_CapitolTrades_Data.csv : Unfiltered/unclean data pulled straight from [CapitolTrades](https://www.capitoltrades.com/).
- Scrubbed_All_CapitolTrades_Data.csv : Clean politician data.
- Stock_Prices_For_All_CapitolTrades_Tickers.csv : Stock data for companies found in CapitolTrades.
3) **Visualizations** : Contains visualizations we created throughout the project.
5) **Team-61.Rproj** : Downloading and opening this file will open RStudio with all our files in the "Files" space. 
6) **Progress Report/Project Proposal/FinalReport** : Contains pdfs of our final deliverables for each phase of the project. 
7) **Final Walkthrough (.rmd/.pdf)** : Files for viewing a general walkthrough of our analysis.
