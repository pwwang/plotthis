# Show the color palettes

This function displays color palettes using ggplot2.

## Usage

``` r
show_palettes(
  palettes = NULL,
  type = c("discrete", "continuous"),
  index = NULL,
  palette_names = NULL,
  return_names = TRUE,
  return_palettes = FALSE
)
```

## Arguments

- palettes:

  A list of color palettes. If `NULL`, uses default palettes.

- type:

  A character vector specifying the type of palettes to include. Default
  is "discrete".

- index:

  A numeric vector specifying the indices of the palettes to include.
  Default is `NULL`.

- palette_names:

  A character vector specifying the names of the SCP palettes to
  include. Default is `NULL`.

- return_names:

  A logical value indicating whether to return the names of the selected
  palettes. Default is `TRUE`.

- return_palettes:

  A logical value indicating whether to return the colors of selected
  palettes. Default is `FALSE`.

## Value

A list of palette names or a list of palettes.

## See also

[`palette_list`](https://pwwang.github.io/plotthis/reference/palette_list.md)

## Examples

``` r
show_palettes(palettes = list(c("red", "blue", "green"), c("yellow", "purple", "orange")))

#> [1] "1" "2"
all_palettes <- show_palettes(return_palettes = TRUE)

names(all_palettes)
#>   [1] "BrBG"                            "PiYG"                           
#>   [3] "PRGn"                            "PuOr"                           
#>   [5] "RdBu"                            "RdGy"                           
#>   [7] "RdYlBu"                          "RdYlGn"                         
#>   [9] "Spectral"                        "Accent"                         
#>  [11] "Dark2"                           "Paired"                         
#>  [13] "Pastel1"                         "Pastel2"                        
#>  [15] "Set1"                            "Set2"                           
#>  [17] "Set3"                            "Blues"                          
#>  [19] "BuGn"                            "BuPu"                           
#>  [21] "GnBu"                            "Greens"                         
#>  [23] "Greys"                           "Oranges"                        
#>  [25] "OrRd"                            "PuBu"                           
#>  [27] "PuBuGn"                          "PuRd"                           
#>  [29] "Purples"                         "RdPu"                           
#>  [31] "Reds"                            "YlGn"                           
#>  [33] "YlGnBu"                          "YlOrBr"                         
#>  [35] "YlOrRd"                          "npg"                            
#>  [37] "aaas"                            "nejm"                           
#>  [39] "lancet"                          "jama"                           
#>  [41] "bmj"                             "jco"                            
#>  [43] "ucscgb"                          "d3-category10"                  
#>  [45] "d3-category20"                   "d3-category20b"                 
#>  [47] "d3-category20c"                  "observable"                     
#>  [49] "primer"                          "atlassian"                      
#>  [51] "igv"                             "locuszoom"                      
#>  [53] "uchicago-default"                "uchicago-light"                 
#>  [55] "uchicago-dark"                   "cosmic"                         
#>  [57] "simpsons"                        "futurama"                       
#>  [59] "rickandmorty"                    "startrek"                       
#>  [61] "tron"                            "frontiers"                      
#>  [63] "flatui"                          "gsea"                           
#>  [65] "bs5"                             "material-red"                   
#>  [67] "material-pink"                   "material-purple"                
#>  [69] "material-deep-purple"            "material-indigo"                
#>  [71] "material-blue"                   "material-light-blue"            
#>  [73] "material-cyan"                   "material-teal"                  
#>  [75] "material-green"                  "material-light-green"           
#>  [77] "material-lime"                   "material-yellow"                
#>  [79] "material-amber"                  "material-orange"                
#>  [81] "material-deep-orange"            "material-brown"                 
#>  [83] "material-grey"                   "material-blue-grey"             
#>  [85] "tw3"                             "dPBIYlBu"                       
#>  [87] "dPBIYlPu"                        "dPBIPuGn"                       
#>  [89] "dPBIPuOr"                        "dPBIRdBu"                       
#>  [91] "dPBIRdGy"                        "dPBIRdGn"                       
#>  [93] "qMSOStd"                         "qMSO12"                         
#>  [95] "qMSO15"                          "qMSOBuWarm"                     
#>  [97] "qMSOBu"                          "qMSOBu2"                        
#>  [99] "qMSOBuGn"                        "qMSOGn"                         
#> [101] "qMSOGnYl"                        "qMSOYl"                         
#> [103] "qMSOYlOr"                        "qMSOOr"                         
#> [105] "qMSOOrRd"                        "qMSORdOr"                       
#> [107] "qMSORd"                          "qMSORdPu"                       
#> [109] "qMSOPu"                          "qMSOPu2"                        
#> [111] "qMSOMed"                         "qMSOPap"                        
#> [113] "qMSOMrq"                         "qMSOSlp"                        
#> [115] "qMSOAsp"                         "qPBI"                           
#> [117] "sPBIGn"                          "sPBIGy1"                        
#> [119] "sPBIRd"                          "sPBIYl"                         
#> [121] "sPBIGy2"                         "sPBIBu"                         
#> [123] "sPBIOr"                          "sPBIPu"                         
#> [125] "sPBIYlGn"                        "sPBIRdPu"                       
#> [127] "ag_GrnYl"                        "ag_Sunset"                      
#> [129] "ArmyRose"                        "Earth"                          
#> [131] "Fall"                            "Geyser"                         
#> [133] "TealRose"                        "Temps"                          
#> [135] "Tropic"                          "Antique"                        
#> [137] "Bold"                            "Pastel"                         
#> [139] "Prism"                           "Safe"                           
#> [141] "Vivid"                           "BluGrn"                         
#> [143] "BluYl"                           "BrwnYl"                         
#> [145] "Burg"                            "BurgYl"                         
#> [147] "DarkMint"                        "Emrld"                          
#> [149] "Magenta"                         "Mint"                           
#> [151] "OrYel"                           "Peach"                          
#> [153] "PinkYl"                          "Purp"                           
#> [155] "PurpOr"                          "RedOr"                          
#> [157] "Sunset"                          "SunsetDark"                     
#> [159] "Teal"                            "TealGrn"                        
#> [161] "polarnight"                      "snowstorm"                      
#> [163] "frost"                           "aurora"                         
#> [165] "lumina"                          "mountain_forms"                 
#> [167] "silver_mine"                     "lake_superior"                  
#> [169] "victory_bonds"                   "halifax_harbor"                 
#> [171] "moose_pond"                      "algoma_forest"                  
#> [173] "rocky_mountain"                  "red_mountain"                   
#> [175] "baie_mouton"                     "afternoon_prarie"               
#> [177] "magma"                           "inferno"                        
#> [179] "plasma"                          "viridis"                        
#> [181] "cividis"                         "rocket"                         
#> [183] "mako"                            "turbo"                          
#> [185] "ocean.algae"                     "ocean.deep"                     
#> [187] "ocean.dense"                     "ocean.gray"                     
#> [189] "ocean.haline"                    "ocean.ice"                      
#> [191] "ocean.matter"                    "ocean.oxy"                      
#> [193] "ocean.phase"                     "ocean.solar"                    
#> [195] "ocean.thermal"                   "ocean.turbid"                   
#> [197] "ocean.balance"                   "ocean.curl"                     
#> [199] "ocean.delta"                     "ocean.amp"                      
#> [201] "ocean.speed"                     "ocean.tempo"                    
#> [203] "BrowntoBlue.10"                  "BrowntoBlue.12"                 
#> [205] "BluetoDarkOrange.12"             "BluetoDarkOrange.18"            
#> [207] "DarkRedtoBlue.12"                "DarkRedtoBlue.18"               
#> [209] "BluetoGreen.14"                  "BluetoGray.8"                   
#> [211] "BluetoOrangeRed.14"              "BluetoOrange.10"                
#> [213] "BluetoOrange.12"                 "BluetoOrange.8"                 
#> [215] "LightBluetoDarkBlue.10"          "LightBluetoDarkBlue.7"          
#> [217] "Categorical.12"                  "GreentoMagenta.16"              
#> [219] "SteppedSequential.5"             "jcolors-default"                
#> [221] "jcolors-pal2"                    "jcolors-pal3"                   
#> [223] "jcolors-pal4"                    "jcolors-pal5"                   
#> [225] "jcolors-pal6"                    "jcolors-pal7"                   
#> [227] "jcolors-pal8"                    "jcolors-pal9"                   
#> [229] "jcolors-pal10"                   "jcolors-pal11"                  
#> [231] "jcolors-pal12"                   "jcolors-rainbow"                
#> [233] "alphabet"                        "alphabet2"                      
#> [235] "glasbey"                         "polychrome"                     
#> [237] "stepped"                         "parade"                         
#> [239] "seurat"                          "seurat.16"                      
#> [241] "seurat.32"                       "seurat.64"                      
#> [243] "jet"                             "simspec"                        
#> [245] "GdRd"                            "stripe"                         
#> [247] "stripe.16"                       "stripe.32"                      
#> [249] "stripe.64"                       "Tableau 10"                     
#> [251] "Tableau 20"                      "Color Blind"                    
#> [253] "Seattle Grays"                   "Traffic"                        
#> [255] "Miller Stone"                    "Superfishel Stone"              
#> [257] "Nuriel Stone"                    "Jewel Bright"                   
#> [259] "Summer"                          "Winter"                         
#> [261] "Green-Orange-Teal"               "Red-Blue-Brown"                 
#> [263] "Purple-Pink-Gray"                "Hue Circle"                     
#> [265] "Classic 10"                      "Classic 10 Medium"              
#> [267] "Classic 10 Light"                "Classic 20"                     
#> [269] "Classic Gray 5"                  "Classic Color Blind"            
#> [271] "Classic Traffic Light"           "Classic Purple-Gray 6"          
#> [273] "Classic Purple-Gray 12"          "Classic Green-Orange 6"         
#> [275] "Classic Green-Orange 12"         "Classic Blue-Red 6"             
#> [277] "Classic Blue-Red 12"             "Classic Cyclic"                 
#> [279] "Orange-Blue Diverging"           "Red-Green Diverging"            
#> [281] "Green-Blue Diverging"            "Red-Blue Diverging"             
#> [283] "Red-Black Diverging"             "Gold-Purple Diverging"          
#> [285] "Red-Green-Gold Diverging"        "Sunset-Sunrise Diverging"       
#> [287] "Orange-Blue-White Diverging"     "Red-Green-White Diverging"      
#> [289] "Green-Blue-White Diverging"      "Red-Blue-White Diverging"       
#> [291] "Red-Black-White Diverging"       "Orange-Blue Light Diverging"    
#> [293] "Temperature Diverging"           "Classic Red-Green"              
#> [295] "Classic Red-Blue"                "Classic Red-Black"              
#> [297] "Classic Area Red-Green"          "Classic Orange-Blue"            
#> [299] "Classic Green-Blue"              "Classic Red-White-Green"        
#> [301] "Classic Red-White-Black"         "Classic Orange-White-Blue"      
#> [303] "Classic Red-White-Black Light"   "Classic Orange-White-Blue Light"
#> [305] "Classic Red-White-Green Light"   "Classic Red-Green Light"        
#> [307] "Blue-Green Sequential"           "Blue Light"                     
#> [309] "Orange Light"                    "Blue"                           
#> [311] "Orange"                          "Green"                          
#> [313] "Red"                             "Purple"                         
#> [315] "Brown"                           "Gray"                           
#> [317] "Gray Warm"                       "Blue-Teal"                      
#> [319] "Orange-Gold"                     "Green-Gold"                     
#> [321] "Red-Gold"                        "Classic Green"                  
#> [323] "Classic Gray"                    "Classic Blue"                   
#> [325] "Classic Red"                     "Classic Orange"                 
#> [327] "Classic Area Red"                "Classic Area Green"             
#> [329] "Classic Area-Brown"             
all_palettes[["simspec"]]
#>  [1] "#c22b86" "#f769a1" "#fcc5c1" "#253777" "#1d92c0" "#9ec9e1" "#015b33"
#>  [8] "#42aa5e" "#d9f0a2" "#E66F00" "#f18c28" "#FFBB61"
#> attr(,"type")
#> [1] "discrete"
show_palettes(index = 1:10)

#>  [1] "BrBG"     "PiYG"     "PRGn"     "PuOr"     "RdBu"     "RdGy"    
#>  [7] "RdYlBu"   "RdYlGn"   "Spectral" "Accent"  
show_palettes(type = "discrete", index = 1:10)

#>  [1] "Accent"  "Dark2"   "Paired"  "Pastel1" "Pastel2" "Set1"    "Set2"   
#>  [8] "Set3"    "npg"     "aaas"   
show_palettes(type = "continuous", index = 1:10)

#>  [1] "BrBG"     "PiYG"     "PRGn"     "PuOr"     "RdBu"     "RdGy"    
#>  [7] "RdYlBu"   "RdYlGn"   "Spectral" "Blues"   
show_palettes(
    palette_names = c("Paired", "nejm", "simspec", "Spectral", "jet"),
    return_palettes = TRUE
)

#> $Paired
#>  [1] "#A6CEE3" "#1F78B4" "#B2DF8A" "#33A02C" "#FDBF6F" "#FF7F00" "#FB9A99"
#>  [8] "#E31A1C" "#CAB2D6" "#6A3D9A" "#FFFF99" "#B15928"
#> attr(,"type")
#> [1] "discrete"
#> 
#> $nejm
#>      TallPoppy   DeepCerulean           Zest     Eucalyptus WildBlueYonder 
#>      "#BC3C29"      "#0072B5"      "#E18727"      "#20854E"      "#7876B1" 
#>         Gothic        Salomie     FrenchRose 
#>      "#6F99AD"      "#FFDC91"      "#EE4C97" 
#> attr(,"type")
#> [1] "discrete"
#> 
#> $simspec
#>  [1] "#c22b86" "#f769a1" "#fcc5c1" "#253777" "#1d92c0" "#9ec9e1" "#015b33"
#>  [8] "#42aa5e" "#d9f0a2" "#E66F00" "#f18c28" "#FFBB61"
#> attr(,"type")
#> [1] "discrete"
#> 
#> $Spectral
#>  [1] "#5E4FA2" "#3288BD" "#66C2A5" "#ABDDA4" "#E6F598" "#FFFFBF" "#FEE08B"
#>  [8] "#FDAE61" "#F46D43" "#D53E4F" "#9E0142"
#> attr(,"type")
#> [1] "continuous"
#> 
#> $jet
#>   [1] "#00007A" "#000085" "#00008F" "#000099" "#0000A3" "#0000AD" "#0000B8"
#>   [8] "#0000C2" "#0000CC" "#0000D6" "#0000E0" "#0000EB" "#0000F5" "#0000FF"
#>  [15] "#000AFF" "#0014FF" "#001FFF" "#0029FF" "#0033FF" "#003DFF" "#0047FF"
#>  [22] "#0052FF" "#005CFF" "#0066FF" "#0070FF" "#007AFF" "#0085FF" "#008FFF"
#>  [29] "#0099FF" "#00A3FF" "#00ADFF" "#00B8FF" "#00C2FF" "#00CCFF" "#00D6FF"
#>  [36] "#00E0FF" "#00EBFF" "#00F5FF" "#00FFFF" "#0AFFF5" "#14FFEB" "#1FFFE0"
#>  [43] "#29FFD6" "#33FFCC" "#3DFFC2" "#47FFB8" "#52FFAD" "#5CFFA3" "#66FF99"
#>  [50] "#70FF8F" "#7AFF85" "#85FF7A" "#8FFF70" "#99FF66" "#A3FF5C" "#ADFF52"
#>  [57] "#B8FF47" "#C2FF3D" "#CCFF33" "#D6FF29" "#E0FF1F" "#EBFF14" "#F5FF0A"
#>  [64] "#FFFF00" "#FFF500" "#FFEB00" "#FFE000" "#FFD600" "#FFCC00" "#FFC200"
#>  [71] "#FFB800" "#FFAD00" "#FFA300" "#FF9900" "#FF8F00" "#FF8500" "#FF7A00"
#>  [78] "#FF7000" "#FF6600" "#FF5C00" "#FF5200" "#FF4700" "#FF3D00" "#FF3300"
#>  [85] "#FF2900" "#FF1F00" "#FF1400" "#FF0A00" "#FF0000" "#F50000" "#EB0000"
#>  [92] "#E00000" "#D60000" "#CC0000" "#C20000" "#B80000" "#AD0000" "#A30000"
#>  [99] "#990000" "#8F0000"
#> attr(,"type")
#> [1] "continuous"
#> 
```
