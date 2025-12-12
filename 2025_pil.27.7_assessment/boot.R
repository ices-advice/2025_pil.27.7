# Script information -----------------------------------------------------------
# Title: pil.27.7
# Authors: Joseph Ribeiro, Rosana Ourens-Chans, Susan Kenyon
# Date created: 2022/05/26
# Purpose: This script should set up your working folder structure,
#          document the origins of any data going in to the analysis (via draft.data function), and
#          bring in the correct versions of packages and any software you need
# Working directory: root (applies to all the TAF .r files)


# Load libraries ---------------------------------------------------------------
library(rstudioapi) # This has been written as an R studio project using RStudio Version 1.4.1717 on windows 10 and R version 4.1.2
library(icesTAF)
library(readxl) # One of the data inputs is consistently provided as an excel spreadsheet
  
# Set the working directory (if sourced in rstudio) ----------------------------
try(setwd(dirname(getActiveDocumentContext()$path)))
wdir = getwd(); print(wdir)
  
# Make the initial dirs / skeleton ---------------------------------------------
taf.skeleton()
mkdir('output')

# Document data and create the data.bib file -----------------------------------
  draft.data(originator="WGHANSA", 
             year=2025, 
             title="Peltic survey index timeseries", 
             period="2017-2025",
             source="Modified from 'Index_boot.csv'. The CV values from that file are applied to the actual biomass index used for advice.
             In previous years, the graph and table was coming from boot means, which is slightly different to the biomass index.
             Subsequently this was used for the graph to avoid getting uneven confidence intervals, but with this new way the confidence
             intervals, tables and advice should match up. This file is updated manually using values sent by email from Jeroen van der Kooij.
             He may also send them in .txt file(s) named ReportBootstrapData.",
             data.files=c("Peltic_timeseries_replacement_datafile.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/Peltic survey index timeseries",
             append=F)
  
  draft.data(originator="WGHANSA", 
             year=2025, 
             title="Landings data for pil.27.7", 
             period="2024",
             source="'Weight in Length class' Intercatch file downloaded in 2025, for 2024. Marked as finalised dataset on 14/11/2025 and downloaded from https://intercatch.ices.dk/CS/Data/StockTransformation/ViewStockDetails.aspx?TaskLogId=28081",
             data.files=c("intercatch_25_MeanWeigthAtAgeLength.txt"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/intercatch_25_MeanWeigthAtAgeLength",
             append=F)
  
  draft.data(originator="WGHANSA", 
             year=2025, 
             title="Landings data for pil.27.7", 
             period="2024",
             source="'Number in Length class' Intercatch file downloaded in 2025, for 2024. Marked as finalised dataset on 14/11/2025 and downloaded from https://intercatch.ices.dk/CS/Data/StockTransformation/ViewStockDetails.aspx?TaskLogId=28081",
             data.files=c("intercatch_25_NumbersAtAgeLength.txt"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/intercatch_25_NumbersAtAgeLength",
             append=F)
  
  draft.data(originator="WGHANSA", 
             year=2025, 
             title="Landings data for pil.27.7", 
             period="2024",
             source="'Stock Overview' Intercatch file downloaded in 2025, for 2024. Marked as finalised dataset on 14/11/2025 and downloaded from https://intercatch.ices.dk/CS/Data/StockTransformation/ViewStockDetails.aspx?TaskLogId=28081",
             data.files=c("intercatch_25_StockOverview.txt"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/intercatch_25_StockOverview",
             append=F)
  
    draft.data(originator="WGHANSA", 
             year=2025, 
             title="Preliminary official landings data for pil", 
             period="2024",
             source="Official landings download for 2024, downloaded by Susan Kenyon from https://data.ices.dk/rec12/login.aspx on 19/11/25 as agreed on 05/12/2022 to replace old offical catch source which was a column in the intercatch data",
             data.files=c("preliminarycatchstatistics_data.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/preliminarycatchstatistics_data",
             append=F)
  
    draft.data(originator="WGHANSA", 
             year=2025, 
             title="Official landings data for pil", 
             period="2006-2023",
             source="Official landings download to 2023, because actually there are 2 different files for official landings, downloaded by Susan Kenyon from https://www.ices.dk/data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx (Official Nominal Catches) on 19/11/25 as agreed on 05/12/2022 to replace old offical catch source which was a column in the intercatch data",
             data.files=c("ICESCatchDataset2006-2023.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/ICESCatchDataset2006-2023",
             append=F)
  
    draft.data(originator="WGHANSA", 
             year=2025, 
             title="Landings data for pil.27.7", 
             period="1987-2024",
             source="Intercatch download pil.27.7_2025-11-14 17_07_59, downloaded by Susan Kenyon from intercatch on 14/11/25 (14. aggregate and export stock data).",
             data.files=c("LandingsOnly.txt"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/LandingsOnly",
             append=F)

  draft.data(originator="WGHANSA", 
             year=2024, 
             title="Landings data for pil.27.7", 
             period="1987-2020",
             source="Provided by FR during WGHANSA 2021 - a correction to apply to the intercatch data, applied in 2021 onwards because intercatch was not updated with the correct data in these years.",
             data.files=c("review landings_France.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/review landings_France",
             append=F)

  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Landings data for pil.27.7", 
             period="1987-2020",
             source="Lifted from old report table. Origin / derivation of this table is not clear to me as it does not align with Catchdata.csv, which is the catch data used to run the spict model",
             data.files=c("official_landings_7.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/official_landings_7",
             append=F)

  draft.data(originator="WGHANSA", 
             year=2022, 
             title="Advice history for pil.27.7, time period a", 
             period="2010-2017",
             source="Lifted from 2022 advice. For this time period (prior to 2017) sardine in this area was assessed as a single stock combining Subarea 7",
             data.files=c("Advice_history_a.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/Advice_history_a",
             append=F)
  
  draft.data(originator="WGHANSA", 
             year=2025, 
             title="Advice history for pil.27.7, time period b", 
             period="2018-2025",
             source="Lifted from 2022 advice. For this time period (from 2017), Sardine was assessed as separate stocks in divisions 8.a–b and 8.d, and Subarea 7.
             Can manually add advice value each year and then the official landings and ICES catches are added within the script.",
             data.files=c("Advice_history_b.csv"), 
             data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
             file="boot/Advice_history_b",
             append=F)

    draft.data(originator="WGHANSA", 
               year=2025, 
               title="French yellow pages", 
               period="2024",
               source="Downloaded from accessions 19/11/2025. Area-WG is sheet used to calculate French landings taken from rectangles 25E4 and 25E5 (Subarea 7 but allocated to Subarea 8)",
               data.files=c("FR_sardine_WG_DAT_france_2024_V2.xls"), 
               data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
               file="boot/FR_sardine_WG_DAT_france_2024_V2",
               append=F)
    
    draft.data(originator="WGHANSA", 
               year=2025, 
               title="PELTIC bootstrap core area", 
               period="2025",
               source="Sent by Jeroen van der Kooij on 21/11/2025. Values manually added to Peltic survey index timeseries spreadsheet.",
               data.files=c("ReportBootstrapData_core.txt"), 
               data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
               file="boot/ReportBootstrapData",
               append=F)
    
    draft.data(originator="WGHANSA", 
               year=2025, 
               title="PELTIC bootstrap core area by age", 
               period="2025",
               source="Sent by Jeroen van der Kooij on 21/11/2025. Not used.",
               data.files=c("ReportBootstrapData_biomage_core.txt"), 
               data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
               file="boot/ReportBootstrapData_biomage_core",
               append=F)
    
    draft.data(originator="WGHANSA", 
               year=2025, 
               title="PELTIC bootstrap full area", 
               period="2025",
               source="Sent by Jeroen van der Kooij on 26/11/2025. Values manually added to Peltic survey index timeseries spreadsheet.",
               data.files=c("ReportBootstrapData_full.txt"), 
               data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
               file="boot/ReportBootstrapData_full",
               append=F)
    
    draft.data(originator="WGHANSA", 
               year=2025, 
               title="PELTIC bootstrap full area by age", 
               period="2025",
               source="Sent by Jeroen van der Kooij on 26/11/2025. Not used.",
               data.files=c("ReportBootstrapData_biomage_full.txt"), 
               data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
               file="boot/ReportBootstrapData_biomage_full",
               append=F)
  
# Upload software --------------------------------------------------------------
# Document software and create the software.bib file
# no software to upload.

# Process the data.bib and software.bib metafiles ------------------------------
# apply taf.boot. Processes the data.bib file. Creates a folder called data. CM showed us that If it can't find a file, use taf.data.path("data.csv") and then run the function & it should work
taf.boot(clean=F)
  unlink('boot/data/reportTemplate.docx') # We don't use the default report template, we use a modified one
  
# Session info -----------------------------------------------------------------
# This is handled at the top of this script by renv
sessionInfo()
  
# End of script ----------------------------------------------------------------
