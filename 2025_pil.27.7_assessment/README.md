# 2025_pil.27.7_assessment
2025 - Sardine (Sardina pilchardus) in Subarea 7 (Southern Celtic Seas, English Channel) - WGHANSA (ADGHANSA)

# Scripts are run in the following order:  
boot.R (creates the .bib files which document data inputs)  
data.R (imports and cleans catch data and creates subsets used for table and figure outputs)  
model.R (imports PELTIC and cleaned catch data, runs the SPiCT model, and plots diagnostics)  
output.R (creates table and figure outputs for the presentation, report, and advice sheet)  
report.R (creates report directory)  

Please note: model input data (Peltic biomass index time series and aggregated quarterly landings time series) are available in the data folder however the code is not set up to run from this folder. 