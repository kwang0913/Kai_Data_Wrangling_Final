# Kai Final  
This is Kailong's Final project of Spring 2018.  
  
## Apr. 14th 2018  
Update Download Import Clean and Merge.  
Dataset is built well.  
  
## Apr. 29th 2018  
If you run "Download and Import.R" and "Clean and Merge.R" you will get 3.2GB dataset.  
The dataset contains citibike data between 2013-07-01 and 2017-12-31.  
If your computer only has 8GB RAM, I recommend use test.RData to see result, which only has data between 
2017-06-01 and 2017-12-31. Otherwise, your computer is supposed to boom shakalaka~  
Open in Browser to get the best display result.  
This shiny app borrows idea from https://github.com/summersuny/01shinyDemo  
  
## May. 7th 2018  
How to use:  
a) Run 'Download and Import.R' to get raw dataset.  
b) Run 'Clean and Merge.R' to Clean dataset and build local SQL server.  
c) Run code in DW_Final folder to see shiny.app result. The folder contains default small size test.Rdata. 
If your machine support (RAM > 12GB), you can use data from step a&b without any problem.     
d) Kailong_Final is a Rmarkdown file for presentation. This file can run independently.  
e) Kailong_Final_Analysis is a Rmarkdown file which need to be run after step a&b.  
  
## June. 22nd 2018  
[Shiny.app](https://kwang0913.shinyapps.io/DW_Final/)  

## July. 2nd 2018  
Re-construct code in DW_Refresh folder. The code is easier to edit and customize than before. Function remains the same.