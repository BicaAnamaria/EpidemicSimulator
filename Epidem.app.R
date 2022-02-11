######################
###
### Epidemic Simulator - Bachelor Thesis
### Student: Anamaria Bica
### West University, Timisoara
### Year 3, Faculty of Computer Science
###
### Coordinator: Daniela Zaharie
### Supervisor: Leonard Mada
### Syonic SRL

### based on:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara


library(shiny)
library(shinyjs)

# library(rsconnect)

# setting project path
setwd("C:\\Users\\anama\\OneDrive - e-uvt.ro\\Documents\\ANUL III\\Semestrul 1\\Licenta\\Simulator")

# load 4 external files with source command

# SIR models, differential equations
source("Epidem.Models.R");
# plot
source("Epidem.Diagrams.R")
# TODO: cleanup / better version
# source("TwoViruses3D(with seed).R")


### Options:
# lty = linetype 
opt.sensitivity.lty = 4;
opt.p.old = 0.2;

### App
# user interface
source("Epidem.UI.R")
# server
source("Epidem.Server.R")

#vwr = dialogViewer('Epidemic models', width = 1600, height = 1300)
#runGadget(shinyApp(ui=ui, server=server), viewer = vwr)
shinyApp(ui=ui, server=server)
