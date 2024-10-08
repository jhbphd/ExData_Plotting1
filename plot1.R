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

rm(householdpower) #removes the large dataset we no longer need

plot1 <- ggplot(HPC, aes(Global_active_power))+
        geom_histogram(boundary = 1, 
                       binwidth = 0.5,color = "black", fill = "red")+ # red bars with black outlines
        ggtitle("Global Active Power")+ # the title
        labs(x = "Global Active Power (kilowatts)", # the axis labels
             y = "Frequency")+
        theme_bw()+ # this removes the gray background
        scale_y_continuous(breaks = c(200, 400, 600, 800, 1000, 1200))+ # sets the ticks with labels
        theme(axis.text.y = element_text(angle = 90), # turns y axis tickmarks 
              panel.grid.major = element_blank(), # no grid
              panel.grid.minor = element_blank(), # no grid
              panel.border = element_blank(), # remove plot border
              axis.line = element_line(), # make sure there is an axis line
              axis.title=element_text(size=10), # text size
              plot.title = element_text(hjust = 0.5)) # centers the title of the plot

print(plot1)
# this saves the plot as a png 5 inches by 5 inches, which is the equivalent of 480x480 pixels digitaly
ggsave("plot1.png", plot = last_plot(), width = 5, height = 5, units = "in")