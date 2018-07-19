# Master script
# Alexey Knorre
# a.v.knorre@gmail.com

eval(parse("./code/0_prepare_data.R", encoding = "UTF-8"))

# Paper
library(rmarkdown)
render("./code/1_paper.Rmd", 
       output_dir = "./results/", output_file = paste0("paper_draft_",Sys.Date(),".docx"),
       encoding = "UTF-8", quiet = T, clean = T, envir = new.env())


# Presentation
#library(rmarkdown)
#render("presentation/presentation_summer_school_irl_2018.Rmd",
#       encoding = "UTF-8", quiet = T, clean = T, envir = new.env())