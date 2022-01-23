######################
###
### Epidemic Simulator
###
### part of:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL


library(shiny)
library(shinyjs)

# library(rsconnect)

# setam calea proiectului

setwd("C:\\Users\\anama\\OneDrive - e-uvt.ro\\Documents\\ANUL III\\Semestrul 1\\Licenta\\Epidem.app.Leo-2021-10-28")

# se incarca 4 fisiere externe cu comanda source

# contine ecuatii diferentiale si modele SIR
source("Epidem.Models.R");
# cod pt generare diagrame in R
source("Epidem.Diagrams.R")
# TODO: cleanup / better version
# source("TwoViruses3D(with seed).R")


### Options:
# lty = linetype 
opt.sensitivity.lty = 4;

### App
# interfacta utilizator
source("Epidem.UI.R")
# cod server
source("Epidem.Server.R")


shinyApp(ui=ui, server=server)

