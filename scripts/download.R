library(googledrive)

for (cur_account in 1:10){
  drive_auth(email = paste0('sotano1pc', cur_account, '@gmail.com'))
  data_files <- drive_find(pattern = 'CCTest2020', type = 'csv')

  if (nrow(data_files)){
    for (cur_row in 1:nrow(data_files)){
      drive_download(file =  data_files[cur_row, ],
                     path = paste0('./input/',  data_files[cur_row, ]$name),
                     overwrite = TRUE)
    }
  }
}
