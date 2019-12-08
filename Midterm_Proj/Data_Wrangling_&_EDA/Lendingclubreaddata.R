pacman::p_load(tidyverse,magrittr,readxl,zoo)
lending_orig <- read_csv("./LendingData/lendingclub_12-15.csv")
list_var <- read_csv("./LendingData/header.csv",col_names = FALSE)
list_var%<>%as.matrix.data.frame()%>%as.vector()
lending_selected <- lending_orig%>%dplyr::select(dplyr::one_of(list_var))
#str(lending_selected)
lending_selected$y <- as.numeric(grepl(pattern = "Charged Off",x = lending_selected$loan_status,ignore.case = TRUE,fixed = TRUE))
lending_selected$int_rate <- as.numeric(gsub("%", "",lending_selected$int_rate))/100

lending_selected$issue_d <- as.Date(as.yearmon(lending_selected$issue_d,format = "%b-%Y"))

lending_selected$earliest_cr_line <- as.Date(as.yearmon(lending_selected$earliest_cr_line,format = "%b-%Y"))
lending_selected$cr_length <- as.numeric(lending_selected$issue_d-lending_selected$earliest_cr_line)
lending_selected%<>%dplyr::mutate_if(is.character,as.factor)
#str(lending_selected)
lending_selected_na_drop <- lending_selected%>%tidyr::drop_na()
## Check if it's correctly labeled
#res <- dplyr::select(lending_selected,loan_status)%>%unique()%>%pull()
#res[5]
#View(dplyr::filter(lending_selected,loan_status %in% res[5]))

## Calculate rate of Charged Off
#lending_selected_na_drop$int_rate <- as.character.factor(lending_selected_na_drop$int_rate)


#sum(lending_selected_na_drop$y)/length(lending_selected_na_drop$y)
## the result is 15%

rm(lending_orig)
