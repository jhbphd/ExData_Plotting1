# This section installs the needed libraries

if (!require("ggplot2")) {
        install.packages("ggplot2")
        library("ggplot2")
}
if (!require("readr")) {
        install.packages("readr")
        library("readr")
}
if (!require("tidyr")) {
        install.packages("tidyr")
        library("tidyr")
}
if (!require("dplyr")) {
        install.packages("dplyr")
        library("dplyr")
}
if (!require("cowplot")) {
        install.packages("cowplot")
        library("cowplot")
}
# this reads the data
householdpower <- read_delim("household_power_consumption.txt", na = c("","?","NA"), show_col_types = FALSE)
# this filters the data and puts the date and time data in a useable and convenient format
HPC <- householdpower%>%
        mutate(Date = as.Date.character(Date, format = c("%d/%m/%Y"))) %>%
        filter(Date %in% c("2007-02-01", "2007-02-02"))%>%
        mutate(DateTime = as.POSIXct(paste(Date, Time)))%>%
        na.omit()

rm(householdpower) #removes the large dataset

plot2<- ggplot(HPC, aes(DateTime, Global_active_power))+
        geom_line()+
        labs(y = "Global Active Power (kilowatts)")+
        theme_bw()+ # removes gray background
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day")+
        theme(axis.title.x = element_blank(), # no x axis title
              axis.text.y = element_text(angle = 90), # turns the y ticks sideways 
              panel.grid.major = element_blank(), # no grid
              panel.grid.minor = element_blank()) # no grid

print(plot2)
# saves the plot as a png 5 inches by 5 inches which is the digital equivalent of 480x480px
ggsave("plot2.png", plot = last_plot(), width = 5, height = 5, units = "in")