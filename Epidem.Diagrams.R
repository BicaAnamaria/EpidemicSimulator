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


diagramV  = function(file = "SIR + Vaccination.png", save.png = FALSE,scaleX = 1/2, scaleY = 1/2) {
  
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
  m[[6,3]] = ""
  m[[8,3]] = ""
  
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
  coord[3,2] = 0.5 #+ 0.4 * scaleY
  
  # Iy
  coord[4,1] = 0.5
  coord[4,2] = 0.5 + 0.2 * scaleY
  
  # Io
  coord[5,1] = 0.5
  coord[5,2] = 0.5 -0.2 * scaleY
  
  # D
  coord[6,1] = 0.5 + 0.3 * scaleX
  coord[6,2] = 0.5 + 0.4 * scaleY
  
  # So
  coord[7,1] = 0.5 - 0.4 * scaleX
  coord[7,2] =0.5 - 0.2 * scaleY
  
  # R
  coord[8,1] = 0.5 + 0.3 * scaleX
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
  curvedarrow(from = c(0.5 + 0.2 * scaleX, 0.5 + 0.05 * scaleY), to = c(0.5 - 0.1 * scaleX, 0.5 + 0.25 * scaleY), lcol = "orange",
              curve = 0.5, arr.pos = 0.95)
  
  # from H to So
  curvedarrow(from = c(0.5 + 0.2 * scaleX, 0.5 - 0.05 * scaleY), to = c(0.5 -0.1 * scaleX, 0.5 - 0.25 *  scaleY), lcol = "orange",
              curve = -0.5, arr.pos = 0.95)     
  
}
diagramV()


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
  m[[7,5]] = ""
  m[[8,4]] = ""
  m[[8,6]] = ""
  m[[10,5]] = ""
  m[[10,6]] = ""
  m[[10, 3]] = ""
  m[[10, 4]] = ""
  
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # V[Y]
  coord[1,1] = 0.5 - 0.4 * scaleX
  coord[1,2] = 0.5 + 0.9 * scaleY
  
  # V[O]
  coord[11,1] = 0.5 - 0.4 * scaleX
  coord[11,2] = 0.5 - 0.9 * scaleY
  
  # S[Y]
  coord[2,1] = 0.5 - 0.55 * scaleX
  coord[2,2] = 0.5 + 0.5 * scaleY
  
  # S[O]
  coord[9,1] = 0.5 - 0.55 * scaleX
  coord[9,2] = 0.5 - 0.5 * scaleY
  
  # H[Y]
  coord[3,1] = 0.5 + 0.2 * scaleX
  coord[3,2] = 0.5 + 0.5 * scaleY
  
  # H[O]
  coord[4,1] = 0.5 + 0.2 * scaleX#0.5 + 0.3 * scaleX
  coord[4,2] = 0.5 - 0.5 * scaleY#0.5 - 0.1 * scaleY
  
  # I[Y]
  coord[5,1] = 0.5 - 0.2 * scaleX
  coord[5,2] = 0.5 + 0.5 * scaleY
  
  # I[O]
  coord[6,1] = 0.5 - 0.2 * scaleX
  coord[6,2] = 0.5 - 0.5 * scaleY
  
  # D[Y]
  coord[7,1] = 0.5 + 0.45 * scaleX
  coord[7,2] =0.5 + 0.9 * scaleY
  
  # D[O]
  coord[8,1] = 0.5 + 0.45 * scaleX
  coord[8,2] = 0.5 - 0.9 * scaleY
  
  # R
  coord[10,1] = 0.5 + 0.45 * scaleX#0.5 + 0.25 * scaleX
  coord[10,2] = 0.5 - 0.1 * scaleY#0.5 - 1 * scaleY
  
 
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.5, curve = 0,
          box.size = 0.017, box.col = color, arr.type = "simple", 
          arr.pos = 0.57, main = "SIR + Vaccination Stratified model")
  
  # the curved arrows (coordinates hard coded)
  # from H[Y] -> I[Y]
  curvedarrow(from = c(0.5 + 0.2 * scaleX,  0.5 + 0.6 * scaleY), to = c(0.5 - 0.3 * scaleX,  0.5 + 0.55 * scaleY), lcol = "red",
              curve =0.9, arr.pos = 0.95)
  
  # from I[Y] -> S[Y]
  curvedarrow(from = c(0.5 - 0.2 * scaleX, 0.5 + 0.4 * scaleY), to = c(0.5 - 0.4 * scaleX,  0.5 + 0.45 * scaleY), lcol = "orange",
              curve =-0.9, arr.pos = 0.95)
  
  # from H[O] to I[O]
  curvedarrow(from = c(0.5 + 0.2 * scaleX, 0.5 - 0.55 * scaleY), to = c(0.5 - 0.3 * scaleX, 0.5 - 0.5 * scaleY), lcol = "red",
              curve = -0.9, arr.pos = 0.95)
  
  # from I[O] to S[O]
  curvedarrow(from = c(0.5 - 0.2 * scaleX, 0.5 - 0.4 * scaleY), to = c(0.5 - 0.4 * scaleX, 0.5 - 0.5 * scaleY), lcol = "orange",
              curve = 0.9, arr.pos = 0.95)     
  
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
  Numgenerations <- 8
  DiffMat <- matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  m <- as.data.frame(DiffMat)
  
  # names and colors of boxes
  name <- c(
    expression(S[Y]), #1
    expression(S[O]), #2
    expression(I[Y]), #3
    expression(I[O]), #4
    expression(H[Y]), #5
    expression(H[O]), #6
    "D", #7
    "R") #8
  
  color <-  c("yellow", "red", "red", "orange", "orange", "dark red", "grey", "green")
  
  # arrows 
  m[[3,1]] = ""
  m[[4,2]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[8,3]] = ""
  m[[6,4]] = ""
  m[[7,4]] = ""
  m[[8,4]] = ""
  m[[7,5]] = ""
  m[[8,5]] = ""
  m[[7,6]] = ""
  m[[8,6]] = ""
  
  # positions of boxes
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Vy
  # coord[1,1] = 0.5 - 0.2 * scaleX
  # coord[1,2] = 0.5 + 0.4 * scaleY
  
  # Sy
  coord[1,1] = 0.5 - 0.4 * scaleX
  coord[1,2] = 0.5 + 0.2 * scaleY
  
  
  # So
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 - 0.2 * scaleY
  
  # Iy
  coord[3,1] = 0.5 - 0.2 * scaleX
  coord[3,2] = 0.5 + 0.2 * scaleY
  
  # Io
  coord[4,1] = 0.5 - 0.2 * scaleX
  coord[4,2] = 0.5 - 0.2 * scaleY
  
  # Hy
  coord[5,1] = 0.5 #+ 0.1 * scaleX
  coord[5,2] = 0.5 + 0.2 * scaleY
  
  # Ho
  coord[6,1] = 0.5 #+ 0.1 * scaleX
  coord[6,2] = 0.5 - 0.2 * scaleY
  
  # D
  coord[7,1] = 0.5 + 0.2 * scaleX
  coord[7,2] = 0.5 + 0.4 * scaleY
  
  
  # R
  coord[8,1] = 0.5 + 0.2 * scaleY
  coord[8,2] = 0.5 - 0.4 * scaleY
  
  # Vo
  # coord[9,1] = 0.5 - 0.2 * scaleX
  #coord[9,2] = 0.5 - 0.4 * scaleY
  
  # plotting the diagram
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.25, curve = 0,
          box.size = 0.021, box.col = color, arr.type = "simple", 
          arr.pos = 0.7, main = "SIR + Vaccination model")
  
  # the curved arrows (coordinates hard coded)
  # from Inf young to SusYoung
  curvedarrow(from = c(0.5 - 0.2 * scaleX, 0.5 + 0.25 * scaleY), to = c(0.5 - 0.3 * scaleX, 0.5 + 0.2 * scaleY), lcol = "red",
              curve =0.7, arr.pos = 0.95)
  
  # from Inf old to SusOld
  curvedarrow(from = c(0.5- 0.2 * scaleX, 0.5 -0.25 * scaleY), to = c(0.5 - 0.3 * scaleX, 0.5 - 0.22 * scaleY), lcol = "red",
              curve =-0.7, arr.pos = 0.95)
  
  # from H to Sy
  curvedarrow(from = c(0.5 , 0.5 + 0.25 * scaleY), to = c(0.5 - 0.3 * scaleX, 0.5 + 0.2 * scaleY), lcol = "orange",
              curve = 0.9, arr.pos = 0.9)
  
  # from H to So
  curvedarrow(from = c(0.5 , 0.5 - 0.25 * scaleY), to = c(0.5 - 0.3 * scaleX, 0.5 - 0.2 * scaleY), lcol = "orange",
              curve = -0.9, arr.pos = 0.9)     
  
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
  Numgenerations <- 9;
  Diffmatrix = matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  
  m <- as.data.frame(Diffmatrix)
  
  # colori si nume cercuri
  name <- c("S", #1
            expression(IV1[1]), #2
            expression(IV2[2]), #3 
            expression(HV1[1]), #4 
            expression(HV2[2]), #5
            expression(DV1[2]), #6 
            expression(DV2[1]), #7
            expression(RV1[1]), #8
            expression(RV2[2])  #9
             
            )
  color <-  c("light green", "yellow", "yellow", "red", "red", "grey", "grey", "green", "light green")
  
  #sageti
  m[[2,1]] = ""
  m[[3,1]] = ""
  m[[4,2]] = ""
  m[[6,2]] = ""
  m[[8,2]] = ""
  m[[5,3]] = ""
  m[[7,3]] = ""
  m[[9,3]] = ""
  m[[6,4]] = ""
  m[[8,4]] = ""
  m[[7,5]] = ""
  m[[9,5]] = ""
  
  
  #pozitii
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # S
  coord[1,1] = 0.5 -0.8 * scaleX
  coord[1,2] = 0.5 -0.2 * scaleY
  
  # IV1[1]
  
  coord[2,1] = 0.5 - 0.4 * scaleX
  coord[2,2] = 0.5 + 0.2 * scaleY
  
  # IV2[2]
  
  coord[3,1] = 0.5 -0.4 * scaleX
  coord[3,2] = 0.5 -0.5 * scaleY
  
  # HV1[1]
  
  coord[4,1] = 0.5 - 0.25 * scaleX
  coord[4,2] = 0.5 + 0.8 * scaleY
  
  # HV2[2]
  
  coord[5,1] = 0.5 - 0.1 * scaleX 
  coord[5,2] = 0.5 - 1.3 * scaleY
  
  # DV1[1]
  
  coord[6,1] = 0.5 + 0.3 * scaleX
  coord[6,2] = 0.5 + 0.9 * scaleY
  
  # DV2[2]
  
  coord[7,1] = 0.5 + 0.3 * scaleX
  coord[7,2] = 0.5 - 1.2 * scaleY
  
  # RV1[1]
  
  coord[9,1] = 0.5 + 0.3 * scaleX
  coord[9,2] = 0.5 - 0.5 * scaleY
  
  # RV2[2]
  
  coord[8,1] = 0.5 + 0.3 * scaleX
  coord[8,2] = 0.5 + 0.2 * scaleY
  

  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.5, curve = 0,
          box.size = 0.02, box.col = color, arr.type = "simple", 
          arr.pos = 0.6, main = "2 Viruses")
  
  # from IV1 -> SV1
  curvedarrow(from = c(0.5 - 0.45 * scaleX,  0.5 + 0.3 * scaleY), to = c(0.5 - 0.6 * scaleX,  0.5 ), lcol = "orange",
              curve =0.4, arr.pos = 0.9)
  
  # from IV2 -> SV2
  curvedarrow(from = c(0.5 - 0.45 * scaleX,  0.5 - 0.6 * scaleY), to = c(0.5 - 0.6 * scaleX,  0.5 - 0.3 * scaleY), lcol = "orange",
              curve =-0.3, arr.pos = 0.9)

  
  }
diagram.2V()

############################
####Diagram Age Groups######
############################


diagram.AG3 = function(file = "Age Groups Model.png", save.png = FALSE, scaleX = 1/3, scaleY = 1/3){
  
  if(save.png) {
    # run this to save as png;
    png(file = file, width = 11.7, height = 8.3, units = "in", res=100)
  } else {
    #dev.new(width = 11.7, height = 8.3)
  }
  
  # numarul de categorii ale modelului
  Numgenerations <- 12;
  Diffmatrix = matrix(data = 0, nrow = Numgenerations, ncol = Numgenerations)
  
  m <- as.data.frame(Diffmatrix)
  
  # colori si nume cercuri
  name <- c(expression(Sc), #1
            expression(Sa), #2
            expression(Sa), #3 
            expression(Ic), #4 
            expression(Ia), #5
            expression(Io), #6 
            expression(Hc), #7
            expression(Ha), #8
            expression(Ho), #9
            expression(Dc/Rc), #10
            expression(Da/Ra), #11
            expression(Do/Ro) #12
            #expression(Rc), #13
            #expression(Ra), #14
            #expression(Ro)  #15
            
  )
  color <-  c("yellow", "yellow", "yellow", "red", "red", "red", "grey", "grey", "grey", "green", "green", "green", "light green")
  
  #sageti
  m[[4,1]] = ""
  m[[5,2]] = ""
  m[[6,3]] = ""
  m[[7,4]] = ""
  m[[8,5]] = ""
  m[[9,6]] = ""
  m[[10,4]] = ""
  m[[11,5]] = ""
  m[[12,6]] = ""
  m[[10,7]] = ""
  m[[11,8]] = ""
  m[[12,9]] = ""
  
  
  #pozitii
  coord = matrix(nrow = Numgenerations, ncol = 2)
  
  # Sc
  coord[1,1] = 0.5 - 0.9 * scaleX
  coord[1,2] = 0.5 + 1.1 * scaleY
  
  # Sa
  coord[2,1] = 0.5 - 0.9 * scaleX
  coord[2,2] = 0.5 + 0.1 * scaleY
  
  # So
  coord[3,1] = 0.5 - 0.9 * scaleX
  coord[3,2] = 0.5 - 0.9 * scaleY
  
  # Ic
  coord[4,1] = 0.5 - 0.3 * scaleX
  coord[4,2] = 0.5 + 1.1 * scaleY
  
  # Ia
  coord[5,1] = 0.5 - 0.3 * scaleX
  coord[5,2] = 0.5 + 0.1 * scaleY
  
  # Io
  coord[6,1] = 0.5 - 0.3 * scaleX
  coord[6,2] = 0.5 - 0.9 * scaleY
  
  # Hc
  coord[7,1] = 0.5 + 0.3 * scaleX
  coord[7,2] = 0.5 + 1.1 * scaleY
  
  # Ha
  coord[8,1] = 0.5 + 0.3 * scaleX
  coord[8,2] = 0.5 + 0.1 * scaleY
  
  # Ho
  coord[9,1] = 0.5 + 0.3 * scaleX
  coord[9,2] = 0.5 - 0.9 * scaleY
  
  # Dc
  coord[10,1] = 0.5 + 0.9 * scaleX
  coord[10,2] = 0.5 + 2.1 * scaleY
  
  # Da
  coord[11,1] = 0.5 + 0.9 * scaleX
  coord[11,2] = 0.5 + 1 * scaleY
  
  # Do
  coord[12,1] = 0.5 + 0.9 * scaleX
  coord[12,2] = 0.5 #- 0.3 * scaleY
  
  plotmat(A = m, pos = coord, name = name, lwd = 2,
          arr.width = 0.5, curve = 0,
          box.size = 0.018, box.col = color, arr.type = "simple", 
          arr.pos = 0.6, main = "Age Groups Model")
  
}
