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
###
### L. Mada: modified/improved;


### draft v.0.2a
# - split into modules;

################

### Server
server <- function(input, output){
  output$txtVirus = renderText("A model showcasing the spread of two viruses over time")
  output$txtBasic = renderText("Basic Model: (S)usceptible (I)nfectious and (R)emoved model")
  output$txtHosp  = renderText("Complex model: includes a hospitalization compartment")
  output$txtVacc  = renderText("Complex model: includes a vaccination compartment")
  output$txtVaccStratified = renderText("Complex model: inlude age-stratified vaccination model")
  output$txtTwoVirus = renderText("Complex model: includes two viruses")
  
  
  values = reactiveValues();
  # active Tab
  values$Active = "H";
  values$outData = NULL;
  
  # synchronise times across pages
  GetTime = function(type, idInput) {
    if(values$Active == type) {
      valTime = input[[idInput]];
      values$time = valTime;
    } else {
      values$Active = type;
      valTime = values$time;
      updateNumericInput(inputId = idInput, value = valTime);
    }
    return(valTime);
  }
  
  ####################
  
  ### SIR: Basic Model
  output$BasicPl = renderPlot({
    valTime = GetTime("B", "timeB");
    custom = c(input$infectB, input$recovB)
    if(input$toggleB == FALSE)
      initSIR_Basic(custom, valTime)
    else
      diagram1();
  })
  
  ### Extended Model: Old Age + Hospital
  output$Hosp <- renderPlot({
    valTime = GetTime("H", "timeH");
    custom = list(infect = input$infectH,
                  recov.c = input$recov.cH, recov.h = input$recov.hH,
                  death.y = input$death.yH, death.o = input$death.oH,
                  death.h = input$death.hH,
                  hosp.y = input$hosp.yH, hosp.o = input$hosp.oH);
    # Page:
    if(input$toggleH == FALSE) {
      if(input$optSensitivityH == "SIR") {
        outData = initSIR_Hosp(custom, valTime);
        values$outData = outData;
        plotSIR_Hosp(outData, flt=input$optType)
      } else {
        print(custom)
        Sensitivity_Hosp(input$optSensitivityH, custom, valTime, flt=input$optType);
      }
    } else
      diagram.H();
  })
  
  ### Vaccination
  output$Vacc = renderPlot({
    valTime = GetTime("V", "timeV");
    custom = list(infect = input$infectV,
                  recov.c = input$recovV, # c = community, h = hospital
                  recov.h = input$recov.hV,
                  hosp.y = input$hosp.yV,
                  hosp.o = input$hosp.vV,
                  vacc.y = input$vacc.yV / vaccine.rate.scale,
                  vacc.o = input$vacc.oV / vaccine.rate.scale,
                  death.y = input$deathV,
                  death.o = input$death.oV,
                  death.h = input$death.hV
                  )
    if(input$toggleV == FALSE){
      if(input$optSensitivityVacc == "Vacc") {
        outData = initSIR_Vaccine(custom, valTime)
        values$outData = outData;
        plotSIR_Vaccine(outData, flt = input$optTypeV)
      }else {
        idParam = match(input$optSensitivityVacc, c("vacc.y", "vacc.o"));
        max   = if(is.na(idParam)) 1 else 0.005;
        Sensitivity_Vaccine(input$optSensitivityVacc, custom, valTime, max=max, flt=input$optTypeV);
      } 
    }
    else
      diagram3(scaleX=0.9, scaleY=0.9)
    
  })
  
  
  output$VaccS = renderPlot({
    valTime = GetTime("VS", "timeVS");
    custom = list(
      infect = input$infectVS,
      recov.h = input$recov.hVS,
      recov.y = input$recov.yVS,
      recov.o = input$recov.oldVS, # aprox 0.14
      hosp.y = input$hosp.yVS,
      hosp.o = input$hosp.oldVS,
      vacc.o = input$vacc.oVS / vaccine.rate.scale,
      vacc.y = input$vacc.yVS / vaccine.rate.scale,
      death.y = input$death.yVS,
      death.o = input$death.oVS,
      death.hy = input$death.hyVS, 
      death.ho = input$death.hoVS 
      )
    
    if(input$toggleVS == FALSE) {
      if(input$optSensitivityVacc == "VaccStrat") {
        outData = initSIR_VaccineStrat(custom, valTime) 
        values$outData = outData;
        plotSIR_VaccineStrat(outData, flt = input$optTypeVS)
      } else {
        idParam = match(input$optSensitivityVaccStrat, c("vacc.y", "vacc.o"));
        max   = if(is.na(idParam)) 1 else 0.005;
        Sensitivity_VaccineStrat(input$optSensitivityVaccStrat, custom, valTime, max=max, flt=input$optTypeVS);
      }
    } else diagramVS(scaleX=0.4, scaleY=0.4)
    
    
  })
  
  ### 2 Viruses
  output$Virus=renderPlot({
    valTime = GetTime("2V", "time2V");
    custom = list(infectV1 = input$infectV1,
                  infectV2 = input$infectV2,
                  #infectV1V2 = input$infectV1V2,
                  #infectV2V1 = input$infectV2V1,
                  recovV1 = input$recovV1,
                  recovV2 = input$recovV2,
                  recovV1.h = input$recovV1.h,
                  recovV2.h = input$recovV2.h,
                  deathV1 = input$deathV1,
                  deathV2 = input$deathV2,
                  deathV1.h = input$deathV1.h,
                  deathV2.h = input$deathV2.h,
                  hospV1 = input$hospV1,
                  hospV2 = input$hospV2
                  
    )
    if(input$toggle2V == FALSE) {
      outData = initSIR_2Viruses(custom, valTime) 
      values$outData = outData;
      plotSIR_2Viruses(outData)
    }
   # TV3D(input$iters, 2)
  })
  
  output$doStatistics = renderTable({
    summarySIR(values$outData);
  }, align = c('c'))
  
  ### Save Data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Results", ".csv", sep = "");
    },
    content = function(file) {
      # TODO: check first if NULL;
      write.csv(values$outData, file, row.names = FALSE)
    }
  )
}

