# This installs and loads the required packages for this script

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
# This reads in the needed data
householdpower <- read_delim("household_power_consumption.txt", na = c("","?","NA"), show_col_types = FALSE)
# This filters out the unnecessary data
HPC <- householdpower%>%
        mutate(Date = as.Date.character(Date, format = c("%d/%m/%Y"))) %>%
        filter(Date %in% c("2007-02-01", "2007-02-02"))%>%
        mutate(DateTime = as.POSIXct(paste(Date, Time)))%>%
        na.omit()

rm(householdpower) #removes the large dataset
#This creates a secondary data set that will be used for PlotC, making the data tidy.
HPCL <- HPC%>%
        # this makes the sub metering data tidy
        pivot_longer(c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                     names_to = "Sub_metering", values_to = "Energy")%>%
        group_by(Sub_metering) # groups the sub metering data


PlotA <- ggplot(HPC, aes(DateTime, Global_active_power))+
        geom_line(linewidth = 0.25)+
        labs(y="Global Active Power")+ #y axis label
        theme_bw()+ #white background
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day")+ # correct x axis
        theme(axis.text.y = element_text(angle = 90),
              axis.title.x = element_blank(),
              panel.grid.major = element_blank(), # no grid
              panel.grid.minor = element_blank(), # no grid
              axis.title=element_text(size=7)) # changes size of text

PlotB<- ggplot(HPC, aes(DateTime, Voltage))+
        geom_line(linewidth = 0.25)+
        labs(x = "datetime", 
             y = "Voltage\n")+
        theme_bw()+
        scale_y_continuous(breaks = c(234, 238, 242, 246), # sets the ticks with labels
                           minor_breaks = c(236, 240, 244))+
        guides(y = guide_axis(minor.ticks = TRUE))+ # puts ticks at the minor breaks
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day")+ # sets correct tick labels
        theme(axis.text.y = element_text(angle = 90),
              panel.grid.major = element_blank(), # no grid
              panel.grid.minor = element_blank(), # no grid
              axis.title=element_text(size=7)) # text size

PlotC<- ggplot(HPCL, aes(x = DateTime, y = Energy, color = Sub_metering))+
        geom_line(linewidth = 0.25)+
        labs(y = "Energy sub metering")+ #y label
        theme_bw()+ # gives white background
        # sets the colors correctly
        scale_color_manual(values = c("black","red","blue"))+ # sets colors for lines
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day")+ # correct day abbrevs to xticks
        theme(axis.title.x = element_blank(), # no x axis label
              axis.title=element_text(size=7), # sets size of axis label
              axis.text.y = element_text(angle = 90), # rotate tick numbers
              panel.grid.major = element_blank(), # no major grid
              panel.grid.minor = element_blank(), # no minor grid
              legend.title = element_blank(), # removes the legend title
              legend.text = element_text(size = 6),
              legend.position = c(0.75,0.88), # positions the legend within the plot
              #legend.key.spacing.y = (rel(.005)), # determines the spacing within the legend
              legend.key.height = unit(0.25, "cm"),
              legend.key.width = unit(0.50, "cm"),
              legend.justification.right = "top", 
              legend.margin = margin(c(0.5,2,2,2))) # this maybe removes margins on legend

PlotD <- ggplot(HPC, aes(DateTime, Global_reactive_power))+
        geom_line(linewidth = 0.25)+
        labs(x = "datetime",
             y = "Global_reactive_power\n")+ #\n gives a line of space
        theme_bw()+ # white background
        scale_y_continuous(breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5))+ # tick labels
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day")+ # x axis tick labels
        theme(axis.text.y = element_text(angle = 90),
              panel.grid.major = element_blank(), # no grid
              panel.grid.minor = element_blank(), # no grid
              axis.title=element_text(size=7)) # font size

plot4 <- plot_grid(PlotA, PlotB, PlotC, PlotD) # positions the 4 plots in a grid
    


ggsave("plot4.png", plot = last_plot(), width = 5, height = 5, units = "in")


