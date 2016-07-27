module Examples where

import DSL.GraphDSL
import DSL.SolverExports
import DSL.QPNModeler
import Data.Map

tinyExample = do
    a <- mkNode "a"
    b <- mkNode "b"
    c <- mkNode "c"

    -- A temporal feedback loop
    link a >+> b >+> c >~-> a
    constrain $ a := (M, Future)

smallPowerFailure = do -- page 43
    -- nodes
    shortCircuitAtHome       <- mkNode "short circuit at home"
    powerOffAtHome           <- mkNode "power off at home"
    blackOutInNeighbourhood  <- mkNode "black out in neighbourhood"
    -- edges
    link shortCircuitAtHome      >+> powerOffAtHome
    link blackOutInNeighbourhood >+> powerOffAtHome

coastalManagement = do -- page 66 fig. 4.4
    -- nodes
    seaLevelRise    <- mkNode "sea level rise"
    riskOfFlooding  <- mkNode "risk of flooding"
    flooding        <- mkNode "flooding"
    investments     <- mkNode "investments"
    measuresToPreventFlooding <- mkNode "measures to prevent flooding"
    ecologyInTheCoastalZone   <- mkNode "ecology in the coastal zone"
    -- edges
    link seaLevelRise >~+> riskOfFlooding >+> flooding >+> measuresToPreventFlooding
    link measuresToPreventFlooding >-> ecologyInTheCoastalZone
    link measuresToPreventFlooding >~-> riskOfFlooding
    link investments >+> measuresToPreventFlooding

    constrain $ seaLevelRise := (P, Im)
    constrain $ seaLevelRise := (Z, Future)

-- The graph from https://dl.dropboxusercontent.com/u/49395289/Graph%20A.png
exampleGraphWithCycles = do
    a  <- mkNode "a"
    a1 <- mkNode "a1"
    a2 <- mkNode "a2"
    a3 <- mkNode "a3"
    a4 <- mkNode "a4"
    a5 <- mkNode "a5"
    link a >+> a1 >-> a2 >+> a3 >-> a
    link a >+> a4 >+> a5 >+> a
  
    b  <- mkNode "b"
    c  <- mkNode "c"
    d  <- mkNode "d"
    link a >+> b >+> c >+> d
  
    c1 <- mkNode "c1"
    c2 <- mkNode "c2"
    c3 <- mkNode "c3"
    c4 <- mkNode "c4"
    link c >+> c1
    link c2 >+> c1
    link c2 >+> c
    link c >+> c3 >+> c4 >+> c
  
    d1 <- mkNode "d1"
    d2 <- mkNode "d2"
    d3 <- mkNode "d3"
    d4 <- mkNode "d4"
    d5 <- mkNode "d5"
    link d >+> d1 >-> d2 >+> d3 >+> d
    link d >-> d4 >+> d5 >+> d

    constrain $ a := P
    constrain $ c := P

dutchCoastalZone2080 = do
    sand       <- mkNode "Suppletion of sand"
    deltaworks <- mkNode "Deltaworks"
    rivers     <- mkNode "More space for rivers"
    renewables <- mkNode "Need for renewable energy sources"
    construction<-mkNode "Innovations in construction"
    windmills  <- mkNode "Windmills"
    floodrisk  <- mkNode "Risk of flooding"
    perceivedRisk <- mkNode "Perceived risk of flooding"
    dunes      <- mkNode "Wide dune areas"
    nuclear    <- mkNode "Nuclear power"
    safety     <- mkNode "Safety and continuity in coastal zone"
    energyProd <- mkNode "Energy production in coastal zone"
    spatialPres<- mkNode "Spatial pressure"
    qualityLife<- mkNode "Quality of living in coastal zone"
    qualityNature<-mkNode"Quality of nature in coastal zone"
    qualityRecre<-mkNode "Quality of recreation in coastal zone"
    
    constrain $ deltaworks := P
  
    link sand        >-> floodrisk
    link deltaworks  >-> floodrisk
    link rivers      >-> floodrisk
    link renewables  >+> windmills
    link renewables  >+> nuclear
    link floodrisk   >-> safety
    link floodrisk   >+> perceivedRisk
    link dunes       >-> floodrisk
    link dunes       >+> safety
    link perceivedRisk >-> qualityLife
    link construction>+> qualityLife
    link dunes       >+> qualityNature
    link dunes       >+> qualityRecre
    link construction>-> spatialPres
    link spatialPres >-> qualityNature
    link windmills   >-> qualityLife
    link windmills   >-> qualityRecre
    link windmills   >+> energyProd
    link nuclear     >+> energyProd
    link energyProd  >-> qualityNature
    link energyProd  >-> qualityRecre

medicalExample = do
    
    -- Actions
    diet        <- mkNode "Diet"
    exercise    <- mkNode "Exercise"
    b_blockers  <- mkNode "Beta Blockers"
    alcohol     <- mkNode "Alcohol"
    hiking      <- mkNode "Hiking"

    -- External Factors
    history     <- mkNode "Family history of Heart Disease"

    -- Internal Factors
    obesity     <- mkNode "Obesity"
    bp          <- mkNode "Blood Pressure"
    cholesterol <- mkNode "Cholesterol"

    -- Objectives
    fun         <- mkNode "Fun"
    strength    <- mkNode "Strength"
    heart_d     <- mkNode "Heart Disease"

    -- Links
    link diet >--> cholesterol >+> heart_d
    link diet >-> fun
    link history >+> heart_d
    link diet >--> obesity >++> heart_d
    link exercise >-> obesity >++> bp >+> heart_d
    link b_blockers >--> bp
    link alcohol >+> bp
    link alcohol >--> strength
    link alcohol >++> fun
    link hiking >++> fun
    link hiking >-> obesity
    link exercise >++> strength
    link obesity >-> strength

test1 = compile smallPowerFailure
test2 = compile coastalManagement
test3 = compile exampleGraphWithCycles
