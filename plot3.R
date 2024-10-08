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
# filters the needed data
HPC <- householdpower%>%
        mutate(Date = as.Date.character(Date, format = c("%d/%m/%Y"))) %>%
        filter(Date %in% c("2007-02-01", "2007-02-02"))%>%
        mutate(DateTime = as.POSIXct(paste(Date, Time)))%>%
        na.omit()

rm(householdpower) #removes the large dataset thats no longer needed

HPCL <- HPC%>%
        # this makes the sub metering data tidy
        pivot_longer(c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
                     names_to = "Sub_metering", values_to = "Energy")%>%
        group_by(Sub_metering) # groups the sub metering data


plot3 <- ggplot(HPCL, aes(x = DateTime, y = Energy, color = Sub_metering))+
        geom_line(linewidth = 0.25)+
        labs(y = "Energy sub metering")+ #y label
        theme_bw()+ # gives white background
        # sets the colors correctly
        scale_color_manual(values = c("black","red","blue"))+ # sets colors for lines
        scale_x_datetime(date_labels = "%a", date_breaks = "1 day")+ # correct day abbrevs to xticks
        theme(axis.title.x = element_blank(), # no x axis label
              axis.title=element_text(size=10), # sets size of axis label
              axis.text.y = element_text(angle = 90), # rotate tick numbers
              panel.grid.major = element_blank(), # no major grid
              panel.grid.minor = element_blank(), # no minor grid
              legend.title = element_blank(), # removes the legend title
              legend.text = element_text(size = 8),
              legend.position = c(0.86,0.945), # positions the legend within the plot
              #legend.key.spacing.y = (rel(.005)), # determines the spacing within the legend
              legend.key.height = unit(0.4, "cm"),
              legend.key.width = unit(.75, "cm"),
              legend.justification.right = "top", 
              legend.box.background = element_rect(color = "black"),
              legend.margin = margin(c(0.5,2,2,2))) # this removes margins on legend
print(plot3)

# this saves the graph as the correct png file
# I also set it for 5 inches by 5 inches, with 1 inch = 96 pixels as the 
        # standard web conversion. This would result in a 480 x 480 pixel graph 
ggsave("plot3.png", plot = last_plot(), width = 5, height = 5, units = "in")