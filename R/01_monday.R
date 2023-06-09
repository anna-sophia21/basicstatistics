#difference between reading data with tidyverse and baseR
setwd('C:/Users/Home-Office/Documents/PHS-Kurs/basicstatistics')
library(tidyverse)
dat <- read_csv("data/raw/insurance_with_date.csv")
dat1baseR <- read.csv("data/raw/insurance_with_date.csv")

dat2 <- dat |>
  mutate(
    across(c(sex, region), factor),
    gt2_children = children > 2,
    smokes = smoker == "yes",
    date_6m = date + months(6)
  )
str(dat2)

##Data visualization with the tidyverse
#=======================================

install.packages("RColorBrewer")
install.packages("unibeCols", repos = "https://ctu-bern.r-universe.dev")

library(ggplot2)
ggplot(mtcars, aes(x = disp, y = hp)) + #define data and define plot
  geom_point() + #gives Scatterplot
  xlab("displacement (cu. in.)") +
  ylab("power(hp)") +
  ggtitle("Scatter plot in ggplot")

ebola <- read.csv("data/raw/ebola.csv")
str(ebola)

#4A:
#format column datum of ebola as date
ebola$Date <- as.Date(ebola$Date) 
ebola <- arrange(ebola, Date) #sort ebola by date
head(ebola)


#filter data_ebola: cumulative number of confirmed cases in Guinea, Liberia and Sierra Leone before 31 March 2015

ebolacases <- ebola |>
  select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) |>
  filter(date <= as.Date("2015-03-31") & 
           (country == "Guinea" | country == "Liberia" | country == "Sierra Leone"))

#4B: create basic point, line and column plots
plot_ebola_point_v0 <- ggplot(data = ebolacases, 
                        mapping = aes(x = date, y =cum_conf_cases))+
  geom_point()

plot_ebola_line_v0 <- ggplot(data = ebolacases, 
                        mapping = aes(x = date, y =cum_conf_cases))+
  geom_line(mapping = aes(group = country))

plot_ebola_col_v0 <- ggplot(data = ebolacases, 
                             mapping = aes(x = date, y =cum_conf_cases))+
  geom_col(position = "stack")

#4C: colour & fill
ggplot(data = ebolacases, #Pointplot  
                              mapping = aes(x = date, y =cum_conf_cases))+
  geom_point(alpha = 0.9, colour = "pink", fill = "pink", shape = 8, size = 1, stroke = 1)

ggplot(data = ebolacases, #Lineplot
       mapping = aes(x = date, y =cum_conf_cases))+
  geom_line(mapping = aes(group = country),
            alpha = 1.5, colour = "turquoise", linetype = "dashed", linewidth = 0.5)
?geom_line

ggplot(data = ebolacases, #columnplot
       mapping = aes(x = date, y =cum_conf_cases))+
  geom_col(position = "stack", alpha = 0.05, fill = "purple", linetype = 1, linewidht = 0.5, width = 1)

?geom_col

#4D: by country
plot_ebola_point_v1 <-ggplot(data = ebolacases, #Pointplot  
       mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
  geom_point(alpha = 0.7, shape = 21, size = 3, stroke = 1)+
  geom_line(colour = "red")

ggplot(data = ebolacases, #Lineplot
       mapping = aes(x = date, y =cum_conf_cases))+
  geom_line(mapping = aes(group = country, colour = country),
            alpha = 1, linetype = "solid", linewidth = 1)

ggplot(data = ebolacases, #columnplot
       mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
  geom_col(position = "stack", alpha = 0.7, linetype = 1, linewidht = 0.5, width = 1)

#4E: labels
plot_ebola_point_v2 <- ggplot(data = ebolacases, #Pointplot  
       mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
  geom_point(alpha = 0.7, shape = 21, size = 3, stroke = 1)+
  geom_line() +
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")

#4F: change standard colors
library(unibeCols)

plot_ebola_point_v3 <-ggplot(data = ebolacases, #Pointplot  
       mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
  geom_point(alpha = 0.7, shape = 21, size = 2, stroke = 1)+
  geom_line() +
  scale_fill_manual(name = "country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                    values = c("green", "blue", "turquoise"),
                    labels = c("Guinea", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
  scale_colour_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c("green", "blue", "turquoise"),
                      labels = c("Guinea", "Liberia", "Sierra Leone"))+
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")

#4G: Scales
plot_ebola_point_v4 <-ggplot(data = ebolacases, #Pointplot  
       mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
  geom_point(alpha = 0.7, shape = 21, size = 2, stroke = 1)+
  geom_line() +
  scale_fill_manual(name = "country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                    values = c("green", "blue", "turquoise"),
                    labels = c("Guinea", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
  scale_colour_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c("green", "blue", "turquoise"),
                      labels = c("Guinea", "Liberia", "Sierra Leone"))+
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 Dezember", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-02"))) +
   ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")


ggplot(data = ebolacases, #Lineplot
       mapping = aes(x = date, y =cum_conf_cases))+
  geom_line(mapping = aes(group = country, colour = country),
            alpha = 1, linetype = "solid", linewidth = 1)+
  scale_fill_manual(name = "country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                    values = c("red", "blue", "turquoise"),
                    labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
  scale_colour_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                      labels = c("G", "Liberia", "Sierra Leone"))+
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 Dezember", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-02"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500))
  ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases")
  
  
  ggplot(data = ebolacases, #Lineplot
         mapping = aes(x = date, y =cum_conf_cases))+
    geom_line(mapping = aes(group = country, colour = country),
              alpha = 1, linetype = "solid", linewidth = 1)+
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
       scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                          limits = c(0, 10000))+
  ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")
  
  
  ggplot(data = ebolacases, #columnplot
         mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
    geom_col(position = "stack", alpha = 0.7, linetype = 1, linewidht = 0.5, width = 1)+
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
    scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
                 labels = c("29 August", "1 October", "1 Dezember", "1 February", "1 April"),
                 limits = as.Date(c("2014-08-28", "2015-04-02"))) +
    scale_y_continuous(breaks = seq(from = 0, to = 15000, by = 2500),
                       limits = c(0, 15000))+
  ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")

#4H: Themes
  plot_ebola_point_v5 <- ggplot(data = ebolacases, #Pointplot  
         mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
    geom_point(alpha = 0.7, shape = 21, size = 3, stroke = 1)+
    geom_line() +
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
    scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
                 labels = c("29 August", "1 October", "1 Dezember", "1 February", "1 April"),
                 limits = as.Date(c("2014-08-28", "2015-04-02"))) +
    ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")+
  theme_bw() + theme(legend.position = "bottom")
  
  ggplot(data = ebolacases, #Lineplot
         mapping = aes(x = date, y =cum_conf_cases))+
    geom_line(mapping = aes(group = country, colour = country),
              alpha = 1, linetype = "solid", linewidth = 1)+
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
    scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                       limits = c(0, 10000))+
    ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")+
  theme_bw() + theme(legend.position = "bottom")
  
  
  ggplot(data = ebolacases, #columnplot
         mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
    geom_col(position = "stack", alpha = 1, linetype = "solid", linewidth = 0.5, width = 0.5)+
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
    scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
                 labels = c("29 August", "1 October", "1 Dezember", "1 February", "1 April"),
                 limits = as.Date(c("2014-08-28", "2015-04-02"))) +
    scale_y_continuous(breaks = seq(from = 0, to = 15000, by = 2500),
                       limits = c(0, 15000))+
    ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")+
  theme_bw() + theme(legend.position = "bottom")
  
#4I: facet
  plot_ebola_point_v6 <-ggplot(data = ebolacases, #Pointplot  
         mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
    geom_point(alpha = 0.7, shape = 21, size = 3, stroke = 1)+
    geom_line() +
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
    scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
                 labels = c("29 August", "1 October", "1 Dezember", "1 February", "1 April"),
                 limits = as.Date(c("2014-08-28", "2015-04-02"))) +
    ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")+
    theme_bw() + theme(legend.position = "bottom")+
  theme(panel.spacing = unit(2, "lines"))+
    facet_grid(cols = vars(country))
  
  
  ggplot(data = ebolacases, #Lineplot
         mapping = aes(x = date, y =cum_conf_cases))+
    geom_line(mapping = aes(group = country, colour = country),
              alpha = 1, linetype = "solid", linewidth = 1)+
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
    scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                       limits = c(0, 10000))+
    ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")+
    theme_bw() + theme(legend.position = "bottom")+
    facet_grid(cols = vars(country))
  
  
  
  ggplot(data = ebolacases, #columnplot
         mapping = aes(x = date, y =cum_conf_cases, fill = country, colour = country))+
    geom_col(position = "stack", alpha = 1, linetype = "solid", linewidth = 0.5, width = 0.5)+
    scale_fill_manual(name = "country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"), #Bezeichnung aus Daten
                      values = c("red", "blue", "turquoise"),
                      labels = c("G", "Liberia", "Sierra Leone"))+ #Bezeichnung für meine Labels
    scale_colour_manual(name = "country",
                        breaks = c("Guinea", "Liberia", "Sierra Leone"),
                        values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                        labels = c("G", "Liberia", "Sierra Leone"))+
    scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
                 labels = c("29 August", "1 October", "1 Dezember", "1 February", "1 April"),
                 limits = as.Date(c("2014-08-28", "2015-04-02"))) +
    scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                       limits = c(0, 10000))+
    ggtitle(label = "Confirmed Ebola cases") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")+
    theme_bw() + theme(legend.position = "bottom")+
    facet_grid(cols = vars(country))
  
  
#4J: grid
library(cowplot)
plot_ebola_point_grid <- plot_grid(plotlist = list(plot_ebola_point_v0,plot_ebola_point_v1, plot_ebola_point_v2, plot_ebola_point_v3, plot_ebola_point_v4, plot_ebola_point_v5),
                                   labels = c("V0", "V1", "V2", "V3", "V4", "V5"), label_size = 12, nrow = 2)
#5
insurance <- read.csv("data/raw/insurance_with_date.csv")
head(insurance)
insurance2 <- insurance |> mutate(children = as.factor(children))
head(insurance2)

#5A
ggplot(data = insurance2,
       mapping = aes(x = bmi, group = sex, colour = sex, fill = sex)) +
  geom_density(alpha = 0.3)+
  scale_fill_manual(name = "",
                    breaks = c("female", "male"),
                    values = c("red", "blue"),
                    labels = c("Female", "Male"))+
  scale_colour_manual(name = "",
                      breaks = c("female", "male"),
                      values = c("red", "blue"),
                      labels = c("Female", "Male"))+
  xlab(label = expression(paste("BMI (kg/",m^2,")")) ) +
  theme(legend.position = "bottom")

ggplot(data = insurance)+
  geom_histogram(aes(x = charges, y = after_stat(density), colour = sex, fill = sex),
                 alpha = 0.4, bins = 100)+
  geom_density(aes(x = charges, colour = sex), linewidth = 1.5)+
  theme(text = element_text(size=20), legend.position = "top")+
  scale_fill_manual(name = "",
                    breaks = c("female", "male"),
                    values = c("red", "blue"),
                    labels = c("Female", "Male"))+
  scale_colour_manual(name = "",
                      breaks = c("female", "male"),
                      values = c("red", "blue"),
                      labels = c("Female", "Male"))+
  xlab(label = "Charges in Dollar") +
  geom_vline(aes(xintercept = median(charges)), color = "red", linewidth = 1.5)

#5B

ggplot(data = insurance, aes(x = age, y = bmi, color = smoker))+
  geom_point() +
  geom_quantile()+
  theme(legend.position = "top")+
  xlab(label = "Age (years")+
  ylab(label = expression(paste("BMI (kg/",m^2,")")) ) +
scale_fill_manual(name = "",
                  breaks = c("no", "yes"),
                  values = c("red", "blue"),
                  labels = c("No", "Yes"))+
scale_colour_manual(name = "",
                    breaks = c("no", "yes"),
                    values = c("red", "blue"),
                    labels = c("No", "Yes"))

#5C
getwd()
library(ggplot2)
library(readr)
insurance <- read.csv("~/PHS-Kurs/basicstatistics/data/raw/insurance_with_date.csv")

ggplot(data = insurance, aes (x = sex, y = charges))+
  geom_boxplot()+
ylab(label = "Charges ($)")



