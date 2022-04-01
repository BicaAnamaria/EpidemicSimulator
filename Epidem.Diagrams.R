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

### based of:
### Team Project 2021
### Students:
###   Dora Calea, Ioana Obreja,
###   Liviu Sopon and Dragos Ursan
###   West University, Timisoara
###
### Supervisor: Leonard Mada
### Syonic SRL


# install.packages(diagram)

library(diagram)

##################
###Schema SIR#####
##################

diagram1  = function(file = "BasicSIR.png", save.png = FALSE) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res = 100)
  } else {
   # dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 3
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c('S', 'I', 'R')
  color <-  c("yellow", "red", "green")
  
  # arrows 
  m[[2,1]] = ""
  m[[3,2]] = ""
  
  # plotting the diagram
  
  plotmat(A = m, pos = 3, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.08, box.col = color, arr.type = "simple", 
          arr.pos = 0.75, main = "SIR model",box.cex = 5)
  
  # the curved arrow (coordinates hard coded)
  curvedarrow(from = c(0.45,0.58), to = c(0.3,0.5), lwd = 2,
              arr.width = 0.4, curve = 0.5, arr.type = "triangle", 
              arr.pos = 0.8, lcol = "red", arr.col = "red")
}

diagram1() 


#############################
####Schema Vaccination#######
#############################


diagram3  = function(file = "SIR + Vaccination.png", save.png = FALSE,scaleX = 1/2, scaleY = 1/2) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units="in", res = 100)
  } else {
    #dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 9
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(expression(V[Y]), #1
            expression(S[Y]), #2
            "H", #3
            expression(I[Y]), #4
            expression(I[O]), #5
            "D", #6
            expression(S[O]), #7
            "R", #8
            expression(V[O])) #9
  
  color <-  c("light green","yellow","orange","red","red","dark red","grey","green","light green")
  
  # arrows 
  m[[1,2]] = ""
  m[[4,2]] = ""
  m[[5,7]] = ""
  m[[9,7]] = ""
  m[[3,4]] = ""
  m[[3,5]] = ""
  m[[6,4]] = ""
  m[[6,5]] = ""
  m[[8,4]] = ""
  m[[8,5]] = ""
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Vy
  coord[1,1] = 0.5 - 0.2 * scaleX
  coord[1,2] = 0.5 + 0.4 * scaleY
  
  # Sy
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 + 0.2 * scaleY
  
  # H
  coord[3,1] = 0.5 + 0.2 * scaleX
  coord[3,2] = 0.5 + 0.4 * scaleY
  
  # Inf young
  coord[4,1] = 0.5
  coord[4,2] = 0.5 + 0.2 * scaleY
  
  # Inf old
  coord[5,1] = 0.5
  coord[5,2] = 0.5 -0.2 * scaleY
  
  # D
  coord[6,1] = 0.5 + 0.4 * scaleX
  coord[6,2] = 0.5
  
  # So
  coord[7,1] = 0.5 - 0.4 * scaleX
  coord[7,2] =0.5 - 0.2 * scaleY
  
  # R
  coord[8,1] = 0.5 + 0.2 * scaleY
  coord[8,2] = 0.5 - 0.4 * scaleY
  
  # Vo
  coord[9,1] = 0.5 - 0.2 * scaleX
  coord[9,2] = 0.5 - 0.4 * scaleY
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.021, box.col = color, arr.type = "simple", 
          arr.pos = 0.85, main = "SIR + Vaccination model")
  
  # the curved arrows (coordinates hard coded)
  # from Inf young to SusYoung
  curvedarrow(from = c(0.5, 0.5 + 0.2 * scaleY), to = c(0.5 - 0.15 * scaleX, 0.5 + 0.2 * scaleY), lcol = "red",
              curve =0.7, arr.pos = 0.95)
  
  # from Inf old to SusOld
  curvedarrow(from = c(0.5, 0.5 -0.25 * scaleY), to = c(0.5 - 0.15 * scaleX, 0.5 - 0.22 * scaleY), lcol = "red",
              curve =-0.7, arr.pos = 0.95)
  
  # from H to Sy
  curvedarrow(from = c(0.5 + 0.18 * scaleX, 0.5 + 0.4 * scaleY), to = c(0.5 - 0.1 * scaleX, 0.5 + 0.3 * scaleY), lcol = "orange",
              curve = 0.2, arr.pos = 0.9)
  
  # from H to So
  curvedarrow(from = c(0.5 + 0.21 * scaleX, 0.5 +0.37 * scaleY), to = c(0.5 -0.1 * scaleX, 0.5 - 0.34 *  scaleY), lcol = "orange",
              curve = -0.2, arr.pos = 0.9)     
  
}
diagram3()


##########################
####Schema VaccStrat######
##########################

diagramVS  = function(file = "SIR + VaccinationAgeStratified.png", save.png = FALSE,scaleX = 3/4, scaleY = 3/4) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units="in", res = 100)
  } else {
    #dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 11
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(expression(V[Y]), #1
            expression(S[Y]), #2
            expression(H[Y]), #3
            expression(H[O]), #4
            expression(I[Y]), #5
            expression(I[O]), #6
            expression(D[Y]), #7
            expression(D[O]), #8
            expression(S[O]), #9
            "R", #10
            expression(V[O])) #11
  
  color <-  c("light green","yellow","orange", "orange", "red","red","dark red", "dark red", "grey","green","light green")
  
  # arrows 
  m[[1,2]] = ""
  m[[5,2]] = ""
  m[[6,9]] = ""
  m[[11,9]] = ""
  m[[3,5]] = ""
  m[[4,6]] = ""
  m[[7,3]] = ""
  m[[8,4]] = ""
  m[[10,5]] = ""
  m[[10,6]] = ""
  
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Vy
  coord[1,1] = 0.5 - 0.8 * scaleX
  coord[1,2] = 0.5 + 0.5 * scaleY
  
  # Sy
  coord[2,1] = 0.5 - 0.55 * scaleX
  coord[2,2] = 0.5 + 0.3 * scaleY
  
  # H[Y]
  coord[3,1] = 0.5 + 0.4 * scaleX
  coord[3,2] = 0.5 + 0.3 * scaleY
  
  # H[O]
  coord[4,1] = 0.5 + 0.4 * scaleX
  coord[4,2] = 0.5 - 0.3 * scaleY
  
  # I[Y]
  coord[5,1] = 0.5 - 0.1 * scaleX
  coord[5,2] = 0.5 + 0.2 * scaleY
  
  # I[O]
  coord[6,1] = 0.5 - 0.2 * scaleX
  coord[6,2] = 0.5 - 0.3 * scaleY
  
  # D[Y]
  coord[7,1] = 0.5 + 0.75 * scaleX
  coord[7,2] =0.5 + 0.2 * scaleY
  
  # D[O]
  coord[8,1] = 0.5 + 0.75 * scaleX
  coord[8,2] = 0.5 - 0.2 * scaleY
  
  # S[O]
  coord[9,1] = 0.5 - 0.55 * scaleX
  coord[9,2] = 0.5 - 0.2 * scaleY
  
  # R
  coord[10,1] = 0.5 + 0.2 * scaleX
  coord[10,2] = 0.5 - 0.6 * scaleY
  
  # V[O]
  coord[11,1] = 0.5 - 0.8 * scaleX
  coord[11,2] = 0.5 - 0.4 * scaleY
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.5, curve = 0,
          box.size = 0.017, box.col = color, arr.type = "simple", 
          arr.pos = 0.57, main = "SIR + Vaccination Stratified model")
  
  # the curved arrows (coordinates hard coded)
  # from H[Y] -> I[Y]
  curvedarrow(from = c(0.5 + 0.4 * scaleX, 0.5 + 0.35 * scaleY), to = c(0.5 - 0.1 * scaleX, 0.5 + 0.25 * scaleY), lcol = "red",
              curve =0.5, arr.pos = 0.9)
  
  # from I[Y] -> S[Y]
  curvedarrow(from = c(0.5 - 0.1 * scaleX, 0.5 + 0.05 * scaleY), to = c(0.5 - 0.35 * scaleX,  0.5 + 0.15 * scaleY), lcol = "orange",
              curve =-0.4, arr.pos = 0.95)
  
  # from H[O] to I[O]
  curvedarrow(from = c(0.5 + 0.4 * scaleX, 0.5 - 0.25 * scaleY), to = c(0.5 - 0.15 * scaleX, 0.5 - 0.15 * scaleY), lcol = "red",
              curve = 0.25, arr.pos = 0.9)
  
  # from I[O] to S[O]
  curvedarrow(from = c(0.5 - 0.2 * scaleX, 0.5 - 0.4 * scaleY), to = c(0.5 - 0.35 * scaleX, 0.5 - 0.3 * scaleY), lcol = "orange",
              curve = -0.5, arr.pos = 0.9)     
  
}
diagramVS()


################################
####Schema Hospitalisation######
################################

diagram.H  = function(file = "SIR + Vaccination.png", save.png = FALSE,scaleX = 3/4, scaleY = 3/4) {
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res = 100)
  } else {
    #dev.new(width = 11.7, height = 8.3)
  }
  
  # number of categories in the sir model
  Numgenerations <- 7
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(
    expression(S[Y]), #1
    "H", #2
    expression(I[Y]), #3
    expression(I[O]), #4
    "D", #5
    expression(S[O]), #6
    "R") #7
  
  color <-  c("yellow", "orange", "red", "red", "dark red", "grey", "green")
  
  # arrows 
  m[[3,1]] = ""
  m[[4,6]] = ""
  m[[2,3]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[2,4]] = ""
  m[[5,4]] = ""
  m[[7,4]] = ""
  m[[7,5]] = ""
  #m[[6,5]] = ""
  #m[[8,4]] = ""
  #m[[8,5]] = ""
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Vy
  # coord[1,1] = 0.5 - 0.2 * scaleX
  # coord[1,2] = 0.5 + 0.4 * scaleY
  
  # Sy
  coord[1,1] = 0.5 - 0.4 * scaleX
  coord[1,2] = 0.5 + 0.2 * scaleY
  
  # H
  coord[2,1] = 0.5 + 0.2 * scaleX
  coord[2,2] = 0.5 + 0.4 * scaleY
  
  # Inf young
  coord[3,1] = 0.5
  coord[3,2] = 0.5 + 0.2 * scaleY
  
  # Inf old
  coord[4,1] = 0.5
  coord[4,2] = 0.5 -0.2 * scaleY
  
  # D
  coord[5,1] = 0.5 + 0.4 * scaleX
  coord[5,2] = 0.5
  
  # So
  coord[6,1] = 0.5 - 0.4 * scaleX
  coord[6,2] =0.5 - 0.2 * scaleY
  
  # R
  coord[7,1] = 0.5 + 0.2 * scaleY
  coord[7,2] = 0.5 - 0.4 * scaleY
  
  # Vo
  # coord[9,1] = 0.5 - 0.2 * scaleX
  #coord[9,2] = 0.5 - 0.4 * scaleY
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.021, box.col = color, arr.type = "simple", 
          arr.pos = 0.85, main = "SIR + Vaccination model")
  
  # the curved arrows (coordinates hard coded)
  # from Inf young to SusYoung
  curvedarrow(from = c(0.5, 0.5 + 0.2 * scaleY), to = c(0.5 - 0.15 * scaleX, 0.5 + 0.2 * scaleY), lcol = "red",
              curve =0.7, arr.pos = 0.95)
  
  # from Inf old to SusOld
  curvedarrow(from = c(0.5, 0.5 -0.25 * scaleY), to = c(0.5 - 0.15 * scaleX, 0.5 - 0.22 * scaleY), lcol = "red",
              curve =-0.7, arr.pos = 0.95)
  
  # from H to Sy
  curvedarrow(from = c(0.5 + 0.18*scaleX, 0.5 + 0.4 * scaleY), to = c(0.5 - 0.1 * scaleX, 0.5 + 0.3 * scaleY), lcol = "orange",
              curve = 0.2, arr.pos = 0.9)
  
  # from H to So
  curvedarrow(from = c(0.5 + 0.21 * scaleX, 0.5 +0.37 * scaleY), to = c(0.5 -0.1 * scaleX, 0.5 - 0.34 * scaleY), lcol = "orange",
              curve = -0.2, arr.pos = 0.9)     
  
}

### Test
diagram.H()



###########################
####Diagram 2 Viruses######
###########################


diagram.2V = function(file = "2 Virusess.png", save.png = FALSE, scaleX = 1/3, scaleY = 1/3){
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res=100)
  } else {
    #dev.new(width = 11.7, height = 8.3)
  }
  
  # numarul de categorii ale modelului
  Numgenerations <- 10;
  Diffmatrix = matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  
  m <- as.data.frame(Diffmatrix)
  
  # colori si nume cercuri
  name <- c("S", #1
            expression(I[1]), #2
            expression(I[2]), #3 
            expression(Q[1]), #4 = QUARANTINED
            expression(Q[2]), #5
            expression(R[1]), #6
            expression(R[2]), #7
            expression(V[2]), #8 = infectati cu v2
            expression(V[1]), #9 = infectati cu v1
            "W" #10 = indivizi care se vindeca de ambii virusi
            
            
            )
  color <-  c("light green", "yellow", "yellow", "red", "red", "grey", "grey", "green", "light green", "white")
  
  #sageti
  m[[2,1]] = ""
  m[[3,1]] = ""
  m[[4,2]] = ""
  m[[6,2]] = ""
  m[[6,4]] = ""
  m[[8,6]] = ""
  m[[10,8]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[7,5]] = ""
  m[[9,7]] = ""
  m[[10,9]] = ""
  
  #pozitii
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # S
  coord[1,1] = 0.5 -0.8 * scaleX
  coord[1,2] = 0.5 -0.2 * scaleY
  
  # I[1]
  
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 + 0.2 * scaleY
  
  # I[2]
  
  coord[3,1] = 0.5 -0.4 * scaleX
  coord[3,2] = 0.5 -0.5 * scaleY
  
  # Q[1]
  
  coord[4,1] = 0.5 - 0.2 * scaleX
  coord[4,2] = 0.5 + 0.4 * scaleY
  
  # Q[2]
  
  coord[5,1] = 0.5 - 0.2 * scaleX 
  coord[5,2] = 0.5 -0.7 * scaleY
  
  # R[1]
  
  coord[6,1] = 0.5 + 0.1 * scaleX
  coord[6,2] = 0.5 + 0.2 * scaleY

  # R[2]
  
  coord[7,1] = 0.5 + 0.1 * scaleX
  coord[7,2] = 0.5 -0.5 * scaleY
    
  # V[2]
  
  coord[8,1] = 0.5 + 0.5 * scaleX
  coord[8,2] = 0.5 + 0.2 * scaleY
  
  # V[1]
  
  coord[9,1] = 0.5 + 0.5 * scaleX
  coord[9,2] = 0.5 - 0.5 * scaleY
  
  # W
  
  coord[10,1] = 0.5 + 0.85 * scaleX
  coord[10,2] = 0.5 -0.2 * scaleY

  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.5, curve = 0,
          box.size = 0.06, box.col = color, arr.type = "simple", 
          arr.pos = 0.6, main = "2 Viruses")
  

  
  }
diagram.2V()

