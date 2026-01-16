# open excel file
CVqLLStab <- readxl::read_excel('C:/Users/Emi/OneDrive/UWA PhD/MonPolicy StockMkt/Code/locasegri/CVs.xlsx',
                          sheet = 'qLLStab')
CVgenS_qLL <- readxl::read_excel('C:/Users/Emi/OneDrive/UWA PhD/MonPolicy StockMkt/Code/locasegri/CVs.xlsx',
                                sheet = 'genS_qLL')
data_CVs <- list('qLLStab' = CVqLLStab,
            'genS_qLL' = CVgenS_qLL)
usethis::use_data(data_CVs, overwrite = TRUE)

#data_sim_biVAR <- readRDS("bivariateVAR.rds")
 
usethis::use_data(data_sim_biVAR, overwrite = TRUE)
