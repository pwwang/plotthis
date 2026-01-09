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

[All available
palettes](https://pwwang.github.io/plotthis/reference/all-palettes.md)

## Examples

``` r
show_palettes(palettes = list(c("red", "blue", "green"), c("yellow", "purple", "orange")))

#> [1] "1" "2"
all_palettes <- show_palettes(return_palettes = TRUE)

names(all_palettes)
#>   [1] "BrBG"                   "PiYG"                   "PRGn"                  
#>   [4] "PuOr"                   "RdBu"                   "RdGy"                  
#>   [7] "RdYlBu"                 "RdYlGn"                 "Spectral"              
#>  [10] "Accent"                 "Dark2"                  "Paired"                
#>  [13] "Pastel1"                "Pastel2"                "Set1"                  
#>  [16] "Set2"                   "Set3"                   "Blues"                 
#>  [19] "BuGn"                   "BuPu"                   "GnBu"                  
#>  [22] "Greens"                 "Greys"                  "Oranges"               
#>  [25] "OrRd"                   "PuBu"                   "PuBuGn"                
#>  [28] "PuRd"                   "Purples"                "RdPu"                  
#>  [31] "Reds"                   "YlGn"                   "YlGnBu"                
#>  [34] "YlOrBr"                 "YlOrRd"                 "npg"                   
#>  [37] "aaas"                   "nejm"                   "lancet"                
#>  [40] "jama"                   "jco"                    "ucscgb"                
#>  [43] "d3-category10"          "d3-category20"          "d3-category20b"        
#>  [46] "d3-category20c"         "igv"                    "locuszoom"             
#>  [49] "uchicago-default"       "uchicago-light"         "uchicago-dark"         
#>  [52] "cosmic"                 "simpsons"               "futurama"              
#>  [55] "rickandmorty"           "startrek"               "tron"                  
#>  [58] "frontiers"              "flatui"                 "gsea"                  
#>  [61] "material-red"           "material-pink"          "material-purple"       
#>  [64] "material-deep-purple"   "material-indigo"        "material-blue"         
#>  [67] "material-light-blue"    "material-cyan"          "material-teal"         
#>  [70] "material-green"         "material-light-green"   "material-lime"         
#>  [73] "material-yellow"        "material-amber"         "material-orange"       
#>  [76] "material-deep-orange"   "material-brown"         "material-grey"         
#>  [79] "material-blue-grey"     "dPBIYlBu"               "dPBIYlPu"              
#>  [82] "dPBIPuGn"               "dPBIPuOr"               "dPBIRdBu"              
#>  [85] "dPBIRdGy"               "dPBIRdGn"               "qMSOStd"               
#>  [88] "qMSO12"                 "qMSO15"                 "qMSOBuWarm"            
#>  [91] "qMSOBu"                 "qMSOBu2"                "qMSOBuGn"              
#>  [94] "qMSOGn"                 "qMSOGnYl"               "qMSOYl"                
#>  [97] "qMSOYlOr"               "qMSOOr"                 "qMSOOrRd"              
#> [100] "qMSORdOr"               "qMSORd"                 "qMSORdPu"              
#> [103] "qMSOPu"                 "qMSOPu2"                "qMSOMed"               
#> [106] "qMSOPap"                "qMSOMrq"                "qMSOSlp"               
#> [109] "qMSOAsp"                "qPBI"                   "sPBIGn"                
#> [112] "sPBIGy1"                "sPBIRd"                 "sPBIYl"                
#> [115] "sPBIGy2"                "sPBIBu"                 "sPBIOr"                
#> [118] "sPBIPu"                 "sPBIYlGn"               "sPBIRdPu"              
#> [121] "ag_GrnYl"               "ag_Sunset"              "ArmyRose"              
#> [124] "Earth"                  "Fall"                   "Geyser"                
#> [127] "TealRose"               "Temps"                  "Tropic"                
#> [130] "Antique"                "Bold"                   "Pastel"                
#> [133] "Prism"                  "Safe"                   "Vivid"                 
#> [136] "BluGrn"                 "BluYl"                  "BrwnYl"                
#> [139] "Burg"                   "BurgYl"                 "DarkMint"              
#> [142] "Emrld"                  "Magenta"                "Mint"                  
#> [145] "OrYel"                  "Peach"                  "PinkYl"                
#> [148] "Purp"                   "PurpOr"                 "RedOr"                 
#> [151] "Sunset"                 "SunsetDark"             "Teal"                  
#> [154] "TealGrn"                "polarnight"             "snowstorm"             
#> [157] "frost"                  "aurora"                 "lumina"                
#> [160] "mountain_forms"         "silver_mine"            "lake_superior"         
#> [163] "victory_bonds"          "halifax_harbor"         "moose_pond"            
#> [166] "algoma_forest"          "rocky_mountain"         "red_mountain"          
#> [169] "baie_mouton"            "afternoon_prarie"       "magma"                 
#> [172] "inferno"                "plasma"                 "viridis"               
#> [175] "cividis"                "rocket"                 "mako"                  
#> [178] "turbo"                  "ocean.algae"            "ocean.deep"            
#> [181] "ocean.dense"            "ocean.gray"             "ocean.haline"          
#> [184] "ocean.ice"              "ocean.matter"           "ocean.oxy"             
#> [187] "ocean.phase"            "ocean.solar"            "ocean.thermal"         
#> [190] "ocean.turbid"           "ocean.balance"          "ocean.curl"            
#> [193] "ocean.delta"            "ocean.amp"              "ocean.speed"           
#> [196] "ocean.tempo"            "BrowntoBlue.10"         "BrowntoBlue.12"        
#> [199] "BluetoDarkOrange.12"    "BluetoDarkOrange.18"    "DarkRedtoBlue.12"      
#> [202] "DarkRedtoBlue.18"       "BluetoGreen.14"         "BluetoGray.8"          
#> [205] "BluetoOrangeRed.14"     "BluetoOrange.10"        "BluetoOrange.12"       
#> [208] "BluetoOrange.8"         "LightBluetoDarkBlue.10" "LightBluetoDarkBlue.7" 
#> [211] "Categorical.12"         "GreentoMagenta.16"      "SteppedSequential.5"   
#> [214] "jcolors-default"        "jcolors-pal2"           "jcolors-pal3"          
#> [217] "jcolors-pal4"           "jcolors-pal5"           "jcolors-pal6"          
#> [220] "jcolors-pal7"           "jcolors-pal8"           "jcolors-pal9"          
#> [223] "jcolors-pal10"          "jcolors-pal11"          "jcolors-pal12"         
#> [226] "jcolors-rainbow"        "jet"                    "simspec"               
#> [229] "GdRd"                   "alphabet"               "alphabet2"             
#> [232] "glasbey"                "polychrome"             "stepped"               
#> [235] "parade"                 "seurat.16"              "seurat.32"             
#> [238] "seurat.64"              "seurat"                 "stripe"                
#> [241] "stripe.16"              "stripe.32"              "stripe.64"             
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
#> [1] "#BC3C29" "#0072B5" "#E18727" "#20854E" "#7876B1" "#6F99AD" "#FFDC91"
#> [8] "#EE4C97"
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
