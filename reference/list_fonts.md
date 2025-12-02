# List fonts

Lists available fonts. This function lists the fonts available in the
`plotthis` package, as well as system fonts and Google fonts if
specified.

## Usage

``` r
list_fonts(
  family_only = FALSE,
  source = c("all", "builtin", "system", "google")
)
```

## Arguments

- family_only:

  Only list the font family names.

- source:

  Source of the fonts to include. Can be one of "builtin", "system",
  "google", or "all". Default is "all".

## Value

A list of font family names, where the names are the source of the fonts
(system or google) if `family_only` is `TRUE`. If `family_only` is
`FALSE`, returns a data frame with font family names and their paths.#'

## Details

This is inspired from the `plummy` package. Those fonts will be
automatically imported when used in
[`theme_this()`](https://pwwang.github.io/plotthis/reference/theme_this.md),
typically via the `theme_args` argument in plotting functions. For
example, `BarPlot(..., theme_args = list(font_family = "Barlow"))`. You
can also use `import_font("Barlow")` to import the font manually before
using it in plots separately. For example,
`BarPlot(...) + theme(text = element_text(family = "Barlow"))`. See
[`import_font()`](https://pwwang.github.io/plotthis/reference/import_font.md)
for more details on importing fonts.

## Examples

``` r
# \donttest{
list_fonts(family_only = TRUE, source = "all")
#> $builtin
#> [1] "LiberationSans"
#> 
#> $system
#>  [1] "D050000L"             "Nimbus Mono PS"       "Nimbus Roman"        
#>  [4] "Nimbus Sans"          "Nimbus Sans Narrow"   "Standard Symbols PS" 
#>  [7] "DejaVu Math TeX Gyre" "Droid Sans Fallback"  "Lato Black"          
#> [10] "Lato"                 "Lato Hairline"        "Lato Heavy"          
#> [13] "Lato Light"           "Lato Medium"          "Lato Semibold"       
#> [16] "Lato Thin"            "Liberation Mono"      "Liberation Sans"     
#> [19] "Liberation Serif"     "Noto Color Emoji"     "Noto Mono"           
#> [22] "Noto Sans Mono"      
#> 
#> $google
#>    [1] "ABeeZee"                           "Abel"                             
#>    [3] "Abhaya Libre"                      "Abril Fatface"                    
#>    [5] "Aclonica"                          "Acme"                             
#>    [7] "Actor"                             "Adamina"                          
#>    [9] "Advent Pro"                        "Aguafina Script"                  
#>   [11] "Akaya Kanadaka"                    "Akaya Telivigala"                 
#>   [13] "Akronim"                           "Aladin"                           
#>   [15] "Alata"                             "Alatsi"                           
#>   [17] "Aldrich"                           "Alef"                             
#>   [19] "Alegreya"                          "Alegreya SC"                      
#>   [21] "Alegreya Sans"                     "Alegreya Sans SC"                 
#>   [23] "Aleo"                              "Alex Brush"                       
#>   [25] "Alfa Slab One"                     "Alice"                            
#>   [27] "Alike"                             "Alike Angular"                    
#>   [29] "Allan"                             "Allerta"                          
#>   [31] "Allerta Stencil"                   "Allison"                          
#>   [33] "Allura"                            "Almarai"                          
#>   [35] "Almendra"                          "Almendra Display"                 
#>   [37] "Almendra SC"                       "Alumni Sans"                      
#>   [39] "Amarante"                          "Amaranth"                         
#>   [41] "Amatic SC"                         "Amethysta"                        
#>   [43] "Amiko"                             "Amiri"                            
#>   [45] "Amita"                             "Anaheim"                          
#>   [47] "Andada Pro"                        "Andika"                           
#>   [49] "Andika New Basic"                  "Angkor"                           
#>   [51] "Annie Use Your Telescope"          "Anonymous Pro"                    
#>   [53] "Antic"                             "Antic Didone"                     
#>   [55] "Antic Slab"                        "Anton"                            
#>   [57] "Antonio"                           "Arapey"                           
#>   [59] "Arbutus"                           "Arbutus Slab"                     
#>   [61] "Architects Daughter"               "Archivo"                          
#>   [63] "Archivo Black"                     "Archivo Narrow"                   
#>   [65] "Are You Serious"                   "Aref Ruqaa"                       
#>   [67] "Arima Madurai"                     "Arimo"                            
#>   [69] "Arizonia"                          "Armata"                           
#>   [71] "Arsenal"                           "Artifika"                         
#>   [73] "Arvo"                              "Arya"                             
#>   [75] "Asap"                              "Asap Condensed"                   
#>   [77] "Asar"                              "Asset"                            
#>   [79] "Assistant"                         "Astloch"                          
#>   [81] "Asul"                              "Athiti"                           
#>   [83] "Atkinson Hyperlegible"             "Atma"                             
#>   [85] "Atomic Age"                        "Aubrey"                           
#>   [87] "Audiowide"                         "Autour One"                       
#>   [89] "Average"                           "Average Sans"                     
#>   [91] "Averia Gruesa Libre"               "Averia Libre"                     
#>   [93] "Averia Sans Libre"                 "Averia Serif Libre"               
#>   [95] "Azeret Mono"                       "B612"                             
#>   [97] "B612 Mono"                         "Bad Script"                       
#>   [99] "Bahiana"                           "Bahianita"                        
#>  [101] "Bai Jamjuree"                      "Bakbak One"                       
#>  [103] "Ballet"                            "Baloo 2"                          
#>  [105] "Baloo Bhai 2"                      "Baloo Bhaijaan 2"                 
#>  [107] "Baloo Bhaina 2"                    "Baloo Chettan 2"                  
#>  [109] "Baloo Da 2"                        "Baloo Paaji 2"                    
#>  [111] "Baloo Tamma 2"                     "Baloo Tammudu 2"                  
#>  [113] "Baloo Thambi 2"                    "Balsamiq Sans"                    
#>  [115] "Balthazar"                         "Bangers"                          
#>  [117] "Barlow"                            "Barlow Condensed"                 
#>  [119] "Barlow Semi Condensed"             "Barriecito"                       
#>  [121] "Barrio"                            "Basic"                            
#>  [123] "Baskervville"                      "Battambang"                       
#>  [125] "Baumans"                           "Bayon"                            
#>  [127] "Be Vietnam Pro"                    "Bebas Neue"                       
#>  [129] "Belgrano"                          "Bellefair"                        
#>  [131] "Belleza"                           "Bellota"                          
#>  [133] "Bellota Text"                      "BenchNine"                        
#>  [135] "Benne"                             "Bentham"                          
#>  [137] "Berkshire Swash"                   "Besley"                           
#>  [139] "Beth Ellen"                        "Bevan"                            
#>  [141] "BhuTuka Expanded One"              "Big Shoulders Display"            
#>  [143] "Big Shoulders Inline Display"      "Big Shoulders Inline Text"        
#>  [145] "Big Shoulders Stencil Display"     "Big Shoulders Stencil Text"       
#>  [147] "Big Shoulders Text"                "Bigelow Rules"                    
#>  [149] "Bigshot One"                       "Bilbo"                            
#>  [151] "Bilbo Swash Caps"                  "BioRhyme"                         
#>  [153] "BioRhyme Expanded"                 "Birthstone"                       
#>  [155] "Birthstone Bounce"                 "Biryani"                          
#>  [157] "Bitter"                            "Black And White Picture"          
#>  [159] "Black Han Sans"                    "Black Ops One"                    
#>  [161] "Blinker"                           "Bodoni Moda"                      
#>  [163] "Bokor"                             "Bona Nova"                        
#>  [165] "Bonbon"                            "Bonheur Royale"                   
#>  [167] "Boogaloo"                          "Bowlby One"                       
#>  [169] "Bowlby One SC"                     "Brawler"                          
#>  [171] "Bree Serif"                        "Brygada 1918"                     
#>  [173] "Bubblegum Sans"                    "Bubbler One"                      
#>  [175] "Buda"                              "Buenard"                          
#>  [177] "Bungee"                            "Bungee Hairline"                  
#>  [179] "Bungee Inline"                     "Bungee Outline"                   
#>  [181] "Bungee Shade"                      "Butcherman"                       
#>  [183] "Butterfly Kids"                    "Cabin"                            
#>  [185] "Cabin Condensed"                   "Cabin Sketch"                     
#>  [187] "Caesar Dressing"                   "Cagliostro"                       
#>  [189] "Cairo"                             "Caladea"                          
#>  [191] "Calistoga"                         "Calligraffitti"                   
#>  [193] "Cambay"                            "Cambo"                            
#>  [195] "Candal"                            "Cantarell"                        
#>  [197] "Cantata One"                       "Cantora One"                      
#>  [199] "Capriola"                          "Caramel"                          
#>  [201] "Carattere"                         "Cardo"                            
#>  [203] "Carme"                             "Carrois Gothic"                   
#>  [205] "Carrois Gothic SC"                 "Carter One"                       
#>  [207] "Castoro"                           "Catamaran"                        
#>  [209] "Caudex"                            "Caveat"                           
#>  [211] "Caveat Brush"                      "Cedarville Cursive"               
#>  [213] "Ceviche One"                       "Chakra Petch"                     
#>  [215] "Changa"                            "Changa One"                       
#>  [217] "Chango"                            "Charm"                            
#>  [219] "Charmonman"                        "Chathura"                         
#>  [221] "Chau Philomene One"                "Chela One"                        
#>  [223] "Chelsea Market"                    "Chenla"                           
#>  [225] "Cherish"                           "Cherry Cream Soda"                
#>  [227] "Cherry Swash"                      "Chewy"                            
#>  [229] "Chicle"                            "Chilanka"                         
#>  [231] "Chivo"                             "Chonburi"                         
#>  [233] "Cinzel"                            "Cinzel Decorative"                
#>  [235] "Clicker Script"                    "Coda"                             
#>  [237] "Coda Caption"                      "Codystar"                         
#>  [239] "Coiny"                             "Combo"                            
#>  [241] "Comfortaa"                         "Comforter"                        
#>  [243] "Comforter Brush"                   "Comic Neue"                       
#>  [245] "Coming Soon"                       "Commissioner"                     
#>  [247] "Concert One"                       "Condiment"                        
#>  [249] "Content"                           "Contrail One"                     
#>  [251] "Convergence"                       "Cookie"                           
#>  [253] "Copse"                             "Corben"                           
#>  [255] "Corinthia"                         "Cormorant"                        
#>  [257] "Cormorant Garamond"                "Cormorant Infant"                 
#>  [259] "Cormorant SC"                      "Cormorant Unicase"                
#>  [261] "Cormorant Upright"                 "Courgette"                        
#>  [263] "Courier Prime"                     "Cousine"                          
#>  [265] "Coustard"                          "Covered By Your Grace"            
#>  [267] "Crafty Girls"                      "Creepster"                        
#>  [269] "Crete Round"                       "Crimson Pro"                      
#>  [271] "Croissant One"                     "Crushed"                          
#>  [273] "Cuprum"                            "Cute Font"                        
#>  [275] "Cutive"                            "Cutive Mono"                      
#>  [277] "DM Mono"                           "DM Sans"                          
#>  [279] "DM Serif Display"                  "DM Serif Text"                    
#>  [281] "Damion"                            "Dancing Script"                   
#>  [283] "Dangrek"                           "Darker Grotesque"                 
#>  [285] "David Libre"                       "Dawning of a New Day"             
#>  [287] "Days One"                          "Dekko"                            
#>  [289] "Dela Gothic One"                   "Delius"                           
#>  [291] "Delius Swash Caps"                 "Delius Unicase"                   
#>  [293] "Della Respira"                     "Denk One"                         
#>  [295] "Devonshire"                        "Dhurjati"                         
#>  [297] "Didact Gothic"                     "Diplomata"                        
#>  [299] "Diplomata SC"                      "Do Hyeon"                         
#>  [301] "Dokdo"                             "Domine"                           
#>  [303] "Donegal One"                       "Dongle"                           
#>  [305] "Doppio One"                        "Dorsa"                            
#>  [307] "Dosis"                             "DotGothic16"                      
#>  [309] "Dr Sugiyama"                       "Duru Sans"                        
#>  [311] "Dynalight"                         "EB Garamond"                      
#>  [313] "Eagle Lake"                        "East Sea Dokdo"                   
#>  [315] "Eater"                             "Economica"                        
#>  [317] "Eczar"                             "El Messiri"                       
#>  [319] "Electrolize"                       "Elsie"                            
#>  [321] "Elsie Swash Caps"                  "Emblema One"                      
#>  [323] "Emilys Candy"                      "Encode Sans"                      
#>  [325] "Encode Sans Condensed"             "Encode Sans Expanded"             
#>  [327] "Encode Sans SC"                    "Encode Sans Semi Condensed"       
#>  [329] "Encode Sans Semi Expanded"         "Engagement"                       
#>  [331] "Englebert"                         "Enriqueta"                        
#>  [333] "Ephesis"                           "Epilogue"                         
#>  [335] "Erica One"                         "Esteban"                          
#>  [337] "Estonia"                           "Euphoria Script"                  
#>  [339] "Ewert"                             "Exo"                              
#>  [341] "Exo 2"                             "Expletus Sans"                    
#>  [343] "Explora"                           "Fahkwang"                         
#>  [345] "Fanwood Text"                      "Farro"                            
#>  [347] "Farsan"                            "Fascinate"                        
#>  [349] "Fascinate Inline"                  "Faster One"                       
#>  [351] "Fasthand"                          "Fauna One"                        
#>  [353] "Faustina"                          "Federant"                         
#>  [355] "Federo"                            "Felipa"                           
#>  [357] "Fenix"                             "Festive"                          
#>  [359] "Finger Paint"                      "Fira Code"                        
#>  [361] "Fira Mono"                         "Fira Sans"                        
#>  [363] "Fira Sans Condensed"               "Fira Sans Extra Condensed"        
#>  [365] "Fjalla One"                        "Fjord One"                        
#>  [367] "Flamenco"                          "Flavors"                          
#>  [369] "Fleur De Leah"                     "Flow Block"                       
#>  [371] "Flow Circular"                     "Flow Rounded"                     
#>  [373] "Fondamento"                        "Fontdiner Swanky"                 
#>  [375] "Forum"                             "Francois One"                     
#>  [377] "Frank Ruhl Libre"                  "Fraunces"                         
#>  [379] "Freckle Face"                      "Fredericka the Great"             
#>  [381] "Fredoka"                           "Fredoka One"                      
#>  [383] "Freehand"                          "Fresca"                           
#>  [385] "Frijole"                           "Fruktur"                          
#>  [387] "Fugaz One"                         "Fuggles"                          
#>  [389] "Fuzzy Bubbles"                     "GFS Didot"                        
#>  [391] "GFS Neohellenic"                   "Gabriela"                         
#>  [393] "Gaegu"                             "Gafata"                           
#>  [395] "Galada"                            "Galdeano"                         
#>  [397] "Galindo"                           "Gamja Flower"                     
#>  [399] "Gayathri"                          "Gelasio"                          
#>  [401] "Gemunu Libre"                      "Genos"                            
#>  [403] "Gentium Basic"                     "Gentium Book Basic"               
#>  [405] "Geo"                               "Georama"                          
#>  [407] "Geostar"                           "Geostar Fill"                     
#>  [409] "Germania One"                      "Gideon Roman"                     
#>  [411] "Gidugu"                            "Gilda Display"                    
#>  [413] "Girassol"                          "Give You Glory"                   
#>  [415] "Glass Antiqua"                     "Glegoo"                           
#>  [417] "Gloria Hallelujah"                 "Glory"                            
#>  [419] "Gluten"                            "Goblin One"                       
#>  [421] "Gochi Hand"                        "Goldman"                          
#>  [423] "Gorditas"                          "Gothic A1"                        
#>  [425] "Gotu"                              "Goudy Bookletter 1911"            
#>  [427] "Gowun Batang"                      "Gowun Dodum"                      
#>  [429] "Graduate"                          "Grand Hotel"                      
#>  [431] "Grandstander"                      "Gravitas One"                     
#>  [433] "Great Vibes"                       "Grechen Fuemen"                   
#>  [435] "Grenze"                            "Grenze Gotisch"                   
#>  [437] "Grey Qo"                           "Griffy"                           
#>  [439] "Gruppo"                            "Gudea"                            
#>  [441] "Gugi"                              "Gupter"                           
#>  [443] "Gurajada"                          "Gwendolyn"                        
#>  [445] "Habibi"                            "Hachi Maru Pop"                   
#>  [447] "Hahmlet"                           "Halant"                           
#>  [449] "Hammersmith One"                   "Hanalei"                          
#>  [451] "Hanalei Fill"                      "Handlee"                          
#>  [453] "Hanuman"                           "Happy Monkey"                     
#>  [455] "Harmattan"                         "Headland One"                     
#>  [457] "Heebo"                             "Henny Penny"                      
#>  [459] "Hepta Slab"                        "Herr Von Muellerhoff"             
#>  [461] "Hi Melody"                         "Hina Mincho"                      
#>  [463] "Hind"                              "Hind Guntur"                      
#>  [465] "Hind Madurai"                      "Hind Siliguri"                    
#>  [467] "Hind Vadodara"                     "Holtwood One SC"                  
#>  [469] "Homemade Apple"                    "Homenaje"                         
#>  [471] "Hubballi"                          "Hurricane"                        
#>  [473] "IBM Plex Mono"                     "IBM Plex Sans"                    
#>  [475] "IBM Plex Sans Arabic"              "IBM Plex Sans Condensed"          
#>  [477] "IBM Plex Sans Devanagari"          "IBM Plex Sans Hebrew"             
#>  [479] "IBM Plex Sans KR"                  "IBM Plex Sans Thai"               
#>  [481] "IBM Plex Sans Thai Looped"         "IBM Plex Serif"                   
#>  [483] "IM Fell DW Pica"                   "IM Fell DW Pica SC"               
#>  [485] "IM Fell Double Pica"               "IM Fell Double Pica SC"           
#>  [487] "IM Fell English"                   "IM Fell English SC"               
#>  [489] "IM Fell French Canon"              "IM Fell French Canon SC"          
#>  [491] "IM Fell Great Primer"              "IM Fell Great Primer SC"          
#>  [493] "Ibarra Real Nova"                  "Iceberg"                          
#>  [495] "Iceland"                           "Imbue"                            
#>  [497] "Imperial Script"                   "Imprima"                          
#>  [499] "Inconsolata"                       "Inder"                            
#>  [501] "Indie Flower"                      "Inika"                            
#>  [503] "Inknut Antiqua"                    "Inria Sans"                       
#>  [505] "Inria Serif"                       "Inspiration"                      
#>  [507] "Inter"                             "Irish Grover"                     
#>  [509] "Island Moments"                    "Istok Web"                        
#>  [511] "Italiana"                          "Italianno"                        
#>  [513] "Itim"                              "Jacques Francois"                 
#>  [515] "Jacques Francois Shadow"           "Jaldi"                            
#>  [517] "JetBrains Mono"                    "Jim Nightshade"                   
#>  [519] "Jockey One"                        "Jolly Lodger"                     
#>  [521] "Jomhuria"                          "Jomolhari"                        
#>  [523] "Josefin Sans"                      "Josefin Slab"                     
#>  [525] "Jost"                              "Joti One"                         
#>  [527] "Jua"                               "Judson"                           
#>  [529] "Julee"                             "Julius Sans One"                  
#>  [531] "Junge"                             "Jura"                             
#>  [533] "Just Another Hand"                 "Just Me Again Down Here"          
#>  [535] "K2D"                               "Kadwa"                            
#>  [537] "Kaisei Decol"                      "Kaisei HarunoUmi"                 
#>  [539] "Kaisei Opti"                       "Kaisei Tokumin"                   
#>  [541] "Kalam"                             "Kameron"                          
#>  [543] "Kanit"                             "Kantumruy"                        
#>  [545] "Karantina"                         "Karla"                            
#>  [547] "Karma"                             "Katibeh"                          
#>  [549] "Kaushan Script"                    "Kavivanar"                        
#>  [551] "Kavoon"                            "Kdam Thmor"                       
#>  [553] "Keania One"                        "Kelly Slab"                       
#>  [555] "Kenia"                             "Khand"                            
#>  [557] "Khmer"                             "Khula"                            
#>  [559] "Kings"                             "Kirang Haerang"                   
#>  [561] "Kite One"                          "Kiwi Maru"                        
#>  [563] "Klee One"                          "Knewave"                          
#>  [565] "KoHo"                              "Kodchasan"                        
#>  [567] "Koh Santepheap"                    "Kolker Brush"                     
#>  [569] "Kosugi"                            "Kosugi Maru"                      
#>  [571] "Kotta One"                         "Koulen"                           
#>  [573] "Kranky"                            "Kreon"                            
#>  [575] "Kristi"                            "Krona One"                        
#>  [577] "Krub"                              "Kufam"                            
#>  [579] "Kulim Park"                        "Kumar One"                        
#>  [581] "Kumar One Outline"                 "Kumbh Sans"                       
#>  [583] "Kurale"                            "La Belle Aurore"                  
#>  [585] "Lacquer"                           "Laila"                            
#>  [587] "Lakki Reddy"                       "Lalezar"                          
#>  [589] "Lancelot"                          "Langar"                           
#>  [591] "Lateef"                            "Lato"                             
#>  [593] "League Gothic"                     "League Script"                    
#>  [595] "League Spartan"                    "Leckerli One"                     
#>  [597] "Ledger"                            "Lekton"                           
#>  [599] "Lemon"                             "Lemonada"                         
#>  [601] "Lexend"                            "Lexend Deca"                      
#>  [603] "Lexend Exa"                        "Lexend Giga"                      
#>  [605] "Lexend Mega"                       "Lexend Peta"                      
#>  [607] "Lexend Tera"                       "Lexend Zetta"                     
#>  [609] "Libre Barcode 128"                 "Libre Barcode 128 Text"           
#>  [611] "Libre Barcode 39"                  "Libre Barcode 39 Extended"        
#>  [613] "Libre Barcode 39 Extended Text"    "Libre Barcode 39 Text"            
#>  [615] "Libre Barcode EAN13 Text"          "Libre Baskerville"                
#>  [617] "Libre Caslon Display"              "Libre Caslon Text"                
#>  [619] "Libre Franklin"                    "Licorice"                         
#>  [621] "Life Savers"                       "Lilita One"                       
#>  [623] "Lily Script One"                   "Limelight"                        
#>  [625] "Linden Hill"                       "Literata"                         
#>  [627] "Liu Jian Mao Cao"                  "Livvic"                           
#>  [629] "Lobster"                           "Lobster Two"                      
#>  [631] "Londrina Outline"                  "Londrina Shadow"                  
#>  [633] "Londrina Sketch"                   "Londrina Solid"                   
#>  [635] "Long Cang"                         "Lora"                             
#>  [637] "Love Light"                        "Love Ya Like A Sister"            
#>  [639] "Loved by the King"                 "Lovers Quarrel"                   
#>  [641] "Luckiest Guy"                      "Lusitana"                         
#>  [643] "Lustria"                           "Luxurious Roman"                  
#>  [645] "Luxurious Script"                  "M PLUS 1"                         
#>  [647] "M PLUS 1 Code"                     "M PLUS 1p"                        
#>  [649] "M PLUS 2"                          "M PLUS Code Latin"                
#>  [651] "M PLUS Rounded 1c"                 "Ma Shan Zheng"                    
#>  [653] "Macondo"                           "Macondo Swash Caps"               
#>  [655] "Mada"                              "Magra"                            
#>  [657] "Maiden Orange"                     "Maitree"                          
#>  [659] "Major Mono Display"                "Mako"                             
#>  [661] "Mali"                              "Mallanna"                         
#>  [663] "Mandali"                           "Manjari"                          
#>  [665] "Manrope"                           "Mansalva"                         
#>  [667] "Manuale"                           "Marcellus"                        
#>  [669] "Marcellus SC"                      "Marck Script"                     
#>  [671] "Margarine"                         "Markazi Text"                     
#>  [673] "Marko One"                         "Marmelad"                         
#>  [675] "Martel"                            "Martel Sans"                      
#>  [677] "Marvel"                            "Mate"                             
#>  [679] "Mate SC"                           "Maven Pro"                        
#>  [681] "McLaren"                           "Mea Culpa"                        
#>  [683] "Meddon"                            "MedievalSharp"                    
#>  [685] "Medula One"                        "Meera Inimai"                     
#>  [687] "Megrim"                            "Meie Script"                      
#>  [689] "Meow Script"                       "Merienda"                         
#>  [691] "Merienda One"                      "Merriweather"                     
#>  [693] "Merriweather Sans"                 "Metal"                            
#>  [695] "Metal Mania"                       "Metamorphous"                     
#>  [697] "Metrophobic"                       "Michroma"                         
#>  [699] "Milonga"                           "Miltonian"                        
#>  [701] "Miltonian Tattoo"                  "Mina"                             
#>  [703] "Miniver"                           "Miriam Libre"                     
#>  [705] "Mirza"                             "Miss Fajardose"                   
#>  [707] "Mitr"                              "Mochiy Pop One"                   
#>  [709] "Mochiy Pop P One"                  "Modak"                            
#>  [711] "Modern Antiqua"                    "Mogra"                            
#>  [713] "Mohave"                            "Molengo"                          
#>  [715] "Molle"                             "Monda"                            
#>  [717] "Monofett"                          "Monoton"                          
#>  [719] "Monsieur La Doulaise"              "Montaga"                          
#>  [721] "Montagu Slab"                      "MonteCarlo"                       
#>  [723] "Montez"                            "Montserrat"                       
#>  [725] "Montserrat Alternates"             "Montserrat Subrayada"             
#>  [727] "Moo Lah Lah"                       "Moon Dance"                       
#>  [729] "Moul"                              "Moulpali"                         
#>  [731] "Mountains of Christmas"            "Mouse Memoirs"                    
#>  [733] "Mr Bedfort"                        "Mr Dafoe"                         
#>  [735] "Mr De Haviland"                    "Mrs Saint Delafield"              
#>  [737] "Mrs Sheppards"                     "Mukta"                            
#>  [739] "Mukta Mahee"                       "Mukta Malar"                      
#>  [741] "Mukta Vaani"                       "Mulish"                           
#>  [743] "Murecho"                           "MuseoModerno"                     
#>  [745] "Mystery Quest"                     "NTR"                              
#>  [747] "Nanum Brush Script"                "Nanum Gothic"                     
#>  [749] "Nanum Gothic Coding"               "Nanum Myeongjo"                   
#>  [751] "Nanum Pen Script"                  "Neonderthaw"                      
#>  [753] "Nerko One"                         "Neucha"                           
#>  [755] "Neuton"                            "New Rocker"                       
#>  [757] "New Tegomin"                       "News Cycle"                       
#>  [759] "Newsreader"                        "Niconne"                          
#>  [761] "Niramit"                           "Nixie One"                        
#>  [763] "Nobile"                            "Nokora"                           
#>  [765] "Norican"                           "Nosifer"                          
#>  [767] "Notable"                           "Nothing You Could Do"             
#>  [769] "Noticia Text"                      "Noto Kufi Arabic"                 
#>  [771] "Noto Music"                        "Noto Naskh Arabic"                
#>  [773] "Noto Nastaliq Urdu"                "Noto Rashi Hebrew"                
#>  [775] "Noto Sans"                         "Noto Sans Adlam"                  
#>  [777] "Noto Sans Adlam Unjoined"          "Noto Sans Anatolian Hieroglyphs"  
#>  [779] "Noto Sans Arabic"                  "Noto Sans Armenian"               
#>  [781] "Noto Sans Avestan"                 "Noto Sans Balinese"               
#>  [783] "Noto Sans Bamum"                   "Noto Sans Bassa Vah"              
#>  [785] "Noto Sans Batak"                   "Noto Sans Bengali"                
#>  [787] "Noto Sans Bhaiksuki"               "Noto Sans Brahmi"                 
#>  [789] "Noto Sans Buginese"                "Noto Sans Buhid"                  
#>  [791] "Noto Sans Canadian Aboriginal"     "Noto Sans Carian"                 
#>  [793] "Noto Sans Caucasian Albanian"      "Noto Sans Chakma"                 
#>  [795] "Noto Sans Cham"                    "Noto Sans Cherokee"               
#>  [797] "Noto Sans Coptic"                  "Noto Sans Cuneiform"              
#>  [799] "Noto Sans Cypriot"                 "Noto Sans Deseret"                
#>  [801] "Noto Sans Devanagari"              "Noto Sans Display"                
#>  [803] "Noto Sans Duployan"                "Noto Sans Egyptian Hieroglyphs"   
#>  [805] "Noto Sans Elbasan"                 "Noto Sans Elymaic"                
#>  [807] "Noto Sans Georgian"                "Noto Sans Glagolitic"             
#>  [809] "Noto Sans Gothic"                  "Noto Sans Grantha"                
#>  [811] "Noto Sans Gujarati"                "Noto Sans Gunjala Gondi"          
#>  [813] "Noto Sans Gurmukhi"                "Noto Sans HK"                     
#>  [815] "Noto Sans Hanifi Rohingya"         "Noto Sans Hanunoo"                
#>  [817] "Noto Sans Hatran"                  "Noto Sans Hebrew"                 
#>  [819] "Noto Sans Imperial Aramaic"        "Noto Sans Indic Siyaq Numbers"    
#>  [821] "Noto Sans Inscriptional Pahlavi"   "Noto Sans Inscriptional Parthian" 
#>  [823] "Noto Sans JP"                      "Noto Sans Javanese"               
#>  [825] "Noto Sans KR"                      "Noto Sans Kaithi"                 
#>  [827] "Noto Sans Kannada"                 "Noto Sans Kayah Li"               
#>  [829] "Noto Sans Kharoshthi"              "Noto Sans Khmer"                  
#>  [831] "Noto Sans Khojki"                  "Noto Sans Khudawadi"              
#>  [833] "Noto Sans Lao"                     "Noto Sans Lepcha"                 
#>  [835] "Noto Sans Limbu"                   "Noto Sans Linear A"               
#>  [837] "Noto Sans Linear B"                "Noto Sans Lisu"                   
#>  [839] "Noto Sans Lycian"                  "Noto Sans Lydian"                 
#>  [841] "Noto Sans Mahajani"                "Noto Sans Malayalam"              
#>  [843] "Noto Sans Mandaic"                 "Noto Sans Manichaean"             
#>  [845] "Noto Sans Marchen"                 "Noto Sans Masaram Gondi"          
#>  [847] "Noto Sans Math"                    "Noto Sans Mayan Numerals"         
#>  [849] "Noto Sans Medefaidrin"             "Noto Sans Meetei Mayek"           
#>  [851] "Noto Sans Meroitic"                "Noto Sans Miao"                   
#>  [853] "Noto Sans Modi"                    "Noto Sans Mongolian"              
#>  [855] "Noto Sans Mono"                    "Noto Sans Mro"                    
#>  [857] "Noto Sans Multani"                 "Noto Sans Myanmar"                
#>  [859] "Noto Sans N Ko"                    "Noto Sans Nabataean"              
#>  [861] "Noto Sans New Tai Lue"             "Noto Sans Newa"                   
#>  [863] "Noto Sans Nushu"                   "Noto Sans Ogham"                  
#>  [865] "Noto Sans Ol Chiki"                "Noto Sans Old Hungarian"          
#>  [867] "Noto Sans Old Italic"              "Noto Sans Old North Arabian"      
#>  [869] "Noto Sans Old Permic"              "Noto Sans Old Persian"            
#>  [871] "Noto Sans Old Sogdian"             "Noto Sans Old South Arabian"      
#>  [873] "Noto Sans Old Turkic"              "Noto Sans Oriya"                  
#>  [875] "Noto Sans Osage"                   "Noto Sans Osmanya"                
#>  [877] "Noto Sans Pahawh Hmong"            "Noto Sans Palmyrene"              
#>  [879] "Noto Sans Pau Cin Hau"             "Noto Sans Phags Pa"               
#>  [881] "Noto Sans Phoenician"              "Noto Sans Psalter Pahlavi"        
#>  [883] "Noto Sans Rejang"                  "Noto Sans Runic"                  
#>  [885] "Noto Sans SC"                      "Noto Sans Samaritan"              
#>  [887] "Noto Sans Saurashtra"              "Noto Sans Sharada"                
#>  [889] "Noto Sans Shavian"                 "Noto Sans Siddham"                
#>  [891] "Noto Sans Sinhala"                 "Noto Sans Sogdian"                
#>  [893] "Noto Sans Sora Sompeng"            "Noto Sans Soyombo"                
#>  [895] "Noto Sans Sundanese"               "Noto Sans Syloti Nagri"           
#>  [897] "Noto Sans Symbols"                 "Noto Sans Symbols 2"              
#>  [899] "Noto Sans Syriac"                  "Noto Sans TC"                     
#>  [901] "Noto Sans Tagalog"                 "Noto Sans Tagbanwa"               
#>  [903] "Noto Sans Tai Le"                  "Noto Sans Tai Tham"               
#>  [905] "Noto Sans Tai Viet"                "Noto Sans Takri"                  
#>  [907] "Noto Sans Tamil"                   "Noto Sans Tamil Supplement"       
#>  [909] "Noto Sans Telugu"                  "Noto Sans Thaana"                 
#>  [911] "Noto Sans Thai"                    "Noto Sans Thai Looped"            
#>  [913] "Noto Sans Tifinagh"                "Noto Sans Tirhuta"                
#>  [915] "Noto Sans Ugaritic"                "Noto Sans Vai"                    
#>  [917] "Noto Sans Wancho"                  "Noto Sans Warang Citi"            
#>  [919] "Noto Sans Yi"                      "Noto Sans Zanabazar Square"       
#>  [921] "Noto Serif"                        "Noto Serif Ahom"                  
#>  [923] "Noto Serif Armenian"               "Noto Serif Balinese"              
#>  [925] "Noto Serif Bengali"                "Noto Serif Devanagari"            
#>  [927] "Noto Serif Display"                "Noto Serif Dogra"                 
#>  [929] "Noto Serif Ethiopic"               "Noto Serif Georgian"              
#>  [931] "Noto Serif Grantha"                "Noto Serif Gujarati"              
#>  [933] "Noto Serif Gurmukhi"               "Noto Serif Hebrew"                
#>  [935] "Noto Serif JP"                     "Noto Serif KR"                    
#>  [937] "Noto Serif Kannada"                "Noto Serif Khmer"                 
#>  [939] "Noto Serif Lao"                    "Noto Serif Malayalam"             
#>  [941] "Noto Serif Myanmar"                "Noto Serif Nyiakeng Puachue Hmong"
#>  [943] "Noto Serif SC"                     "Noto Serif Sinhala"               
#>  [945] "Noto Serif TC"                     "Noto Serif Tamil"                 
#>  [947] "Noto Serif Tangut"                 "Noto Serif Telugu"                
#>  [949] "Noto Serif Thai"                   "Noto Serif Tibetan"               
#>  [951] "Noto Serif Yezidi"                 "Noto Traditional Nushu"           
#>  [953] "Nova Cut"                          "Nova Flat"                        
#>  [955] "Nova Mono"                         "Nova Oval"                        
#>  [957] "Nova Round"                        "Nova Script"                      
#>  [959] "Nova Slim"                         "Nova Square"                      
#>  [961] "Numans"                            "Nunito"                           
#>  [963] "Nunito Sans"                       "Odibee Sans"                      
#>  [965] "Odor Mean Chey"                    "Offside"                          
#>  [967] "Oi"                                "Old Standard TT"                  
#>  [969] "Oldenburg"                         "Ole"                              
#>  [971] "Oleo Script"                       "Oleo Script Swash Caps"           
#>  [973] "Oooh Baby"                         "Open Sans"                        
#>  [975] "Oranienbaum"                       "Orbitron"                         
#>  [977] "Oregano"                           "Orelega One"                      
#>  [979] "Orienta"                           "Original Surfer"                  
#>  [981] "Oswald"                            "Otomanopee One"                   
#>  [983] "Outfit"                            "Over the Rainbow"                 
#>  [985] "Overlock"                          "Overlock SC"                      
#>  [987] "Overpass"                          "Overpass Mono"                    
#>  [989] "Ovo"                               "Oxanium"                          
#>  [991] "Oxygen"                            "Oxygen Mono"                      
#>  [993] "PT Mono"                           "PT Sans"                          
#>  [995] "PT Sans Caption"                   "PT Sans Narrow"                   
#>  [997] "PT Serif"                          "PT Serif Caption"                 
#>  [999] "Pacifico"                          "Padauk"                           
#> [1001] "Palanquin"                         "Palanquin Dark"                   
#> [1003] "Palette Mosaic"                    "Pangolin"                         
#> [1005] "Paprika"                           "Parisienne"                       
#> [1007] "Passero One"                       "Passion One"                      
#> [1009] "Passions Conflict"                 "Pathway Gothic One"               
#> [1011] "Patrick Hand"                      "Patrick Hand SC"                  
#> [1013] "Pattaya"                           "Patua One"                        
#> [1015] "Pavanam"                           "Paytone One"                      
#> [1017] "Peddana"                           "Peralta"                          
#> [1019] "Permanent Marker"                  "Petemoss"                         
#> [1021] "Petit Formal Script"               "Petrona"                          
#> [1023] "Philosopher"                       "Piazzolla"                        
#> [1025] "Piedra"                            "Pinyon Script"                    
#> [1027] "Pirata One"                        "Plaster"                          
#> [1029] "Play"                              "Playball"                         
#> [1031] "Playfair Display"                  "Playfair Display SC"              
#> [1033] "Podkova"                           "Poiret One"                       
#> [1035] "Poller One"                        "Poly"                             
#> [1037] "Pompiere"                          "Pontano Sans"                     
#> [1039] "Poor Story"                        "Poppins"                          
#> [1041] "Port Lligat Sans"                  "Port Lligat Slab"                 
#> [1043] "Potta One"                         "Pragati Narrow"                   
#> [1045] "Praise"                            "Prata"                            
#> [1047] "Preahvihear"                       "Press Start 2P"                   
#> [1049] "Pridi"                             "Princess Sofia"                   
#> [1051] "Prociono"                          "Prompt"                           
#> [1053] "Prosto One"                        "Proza Libre"                      
#> [1055] "Public Sans"                       "Puppies Play"                     
#> [1057] "Puritan"                           "Purple Purse"                     
#> [1059] "Qahiri"                            "Quando"                           
#> [1061] "Quantico"                          "Quattrocento"                     
#> [1063] "Quattrocento Sans"                 "Questrial"                        
#> [1065] "Quicksand"                         "Quintessential"                   
#> [1067] "Qwigley"                           "Qwitcher Grypen"                  
#> [1069] "Racing Sans One"                   "Radley"                           
#> [1071] "Rajdhani"                          "Rakkas"                           
#> [1073] "Raleway"                           "Raleway Dots"                     
#> [1075] "Ramabhadra"                        "Ramaraja"                         
#> [1077] "Rambla"                            "Rammetto One"                     
#> [1079] "Rampart One"                       "Ranchers"                         
#> [1081] "Rancho"                            "Ranga"                            
#> [1083] "Rasa"                              "Rationale"                        
#> [1085] "Ravi Prakash"                      "Readex Pro"                       
#> [1087] "Recursive"                         "Red Hat Display"                  
#> [1089] "Red Hat Mono"                      "Red Hat Text"                     
#> [1091] "Red Rose"                          "Redacted"                         
#> [1093] "Redacted Script"                   "Redressed"                        
#> [1095] "Reem Kufi"                         "Reenie Beanie"                    
#> [1097] "Reggae One"                        "Revalia"                          
#> [1099] "Rhodium Libre"                     "Ribeye"                           
#> [1101] "Ribeye Marrow"                     "Righteous"                        
#> [1103] "Risque"                            "Road Rage"                        
#> [1105] "Roboto"                            "Roboto Condensed"                 
#> [1107] "Roboto Mono"                       "Roboto Serif"                     
#> [1109] "Roboto Slab"                       "Rochester"                        
#> [1111] "Rock 3D"                           "Rock Salt"                        
#> [1113] "RocknRoll One"                     "Rokkitt"                          
#> [1115] "Romanesco"                         "Ropa Sans"                        
#> [1117] "Rosario"                           "Rosarivo"                         
#> [1119] "Rouge Script"                      "Rowdies"                          
#> [1121] "Rozha One"                         "Rubik"                            
#> [1123] "Rubik Beastly"                     "Rubik Mono One"                   
#> [1125] "Ruda"                              "Rufina"                           
#> [1127] "Ruge Boogie"                       "Ruluko"                           
#> [1129] "Rum Raisin"                        "Ruslan Display"                   
#> [1131] "Russo One"                         "Ruthie"                           
#> [1133] "Rye"                               "STIX Two Text"                    
#> [1135] "Sacramento"                        "Sahitya"                          
#> [1137] "Sail"                              "Saira"                            
#> [1139] "Saira Condensed"                   "Saira Extra Condensed"            
#> [1141] "Saira Semi Condensed"              "Saira Stencil One"                
#> [1143] "Salsa"                             "Sanchez"                          
#> [1145] "Sancreek"                          "Sansita"                          
#> [1147] "Sansita Swashed"                   "Sarabun"                          
#> [1149] "Sarala"                            "Sarina"                           
#> [1151] "Sarpanch"                          "Sassy Frass"                      
#> [1153] "Satisfy"                           "Sawarabi Gothic"                  
#> [1155] "Sawarabi Mincho"                   "Scada"                            
#> [1157] "Scheherazade New"                  "Schoolbell"                       
#> [1159] "Scope One"                         "Seaweed Script"                   
#> [1161] "Secular One"                       "Sedgwick Ave"                     
#> [1163] "Sedgwick Ave Display"              "Sen"                              
#> [1165] "Sevillana"                         "Seymour One"                      
#> [1167] "Shadows Into Light"                "Shadows Into Light Two"           
#> [1169] "Shalimar"                          "Shanti"                           
#> [1171] "Share"                             "Share Tech"                       
#> [1173] "Share Tech Mono"                   "Shippori Antique"                 
#> [1175] "Shippori Antique B1"               "Shippori Mincho"                  
#> [1177] "Shippori Mincho B1"                "Shizuru"                          
#> [1179] "Shojumaru"                         "Short Stack"                      
#> [1181] "Shrikhand"                         "Siemreap"                         
#> [1183] "Sigmar One"                        "Signika"                          
#> [1185] "Signika Negative"                  "Simonetta"                        
#> [1187] "Single Day"                        "Sintony"                          
#> [1189] "Sirin Stencil"                     "Six Caps"                         
#> [1191] "Skranji"                           "Slabo 13px"                       
#> [1193] "Slabo 27px"                        "Slackey"                          
#> [1195] "Smokum"                            "Smooch"                           
#> [1197] "Smooch Sans"                       "Smythe"                           
#> [1199] "Sniglet"                           "Snippet"                          
#> [1201] "Snowburst One"                     "Sofadi One"                       
#> [1203] "Sofia"                             "Solway"                           
#> [1205] "Song Myung"                        "Sonsie One"                       
#> [1207] "Sora"                              "Sorts Mill Goudy"                 
#> [1209] "Source Code Pro"                   "Source Sans 3"                    
#> [1211] "Source Sans Pro"                   "Source Serif 4"                   
#> [1213] "Source Serif Pro"                  "Space Grotesk"                    
#> [1215] "Space Mono"                        "Spartan"                          
#> [1217] "Special Elite"                     "Spectral"                         
#> [1219] "Spectral SC"                       "Spicy Rice"                       
#> [1221] "Spinnaker"                         "Spirax"                           
#> [1223] "Spline Sans"                       "Squada One"                       
#> [1225] "Sree Krushnadevaraya"              "Sriracha"                         
#> [1227] "Srisakdi"                          "Staatliches"                      
#> [1229] "Stalemate"                         "Stalinist One"                    
#> [1231] "Stardos Stencil"                   "Stick"                            
#> [1233] "Stick No Bills"                    "Stint Ultra Condensed"            
#> [1235] "Stint Ultra Expanded"              "Stoke"                            
#> [1237] "Strait"                            "Style Script"                     
#> [1239] "Stylish"                           "Sue Ellen Francisco"              
#> [1241] "Suez One"                          "Sulphur Point"                    
#> [1243] "Sumana"                            "Sunflower"                        
#> [1245] "Sunshiney"                         "Supermercado One"                 
#> [1247] "Sura"                              "Suranna"                          
#> [1249] "Suravaram"                         "Suwannaphum"                      
#> [1251] "Swanky and Moo Moo"                "Syncopate"                        
#> [1253] "Syne"                              "Syne Mono"                        
#> [1255] "Syne Tactile"                      "Tajawal"                          
#> [1257] "Tangerine"                         "Taprom"                           
#> [1259] "Tauri"                             "Taviraj"                          
#> [1261] "Teko"                              "Telex"                            
#> [1263] "Tenali Ramakrishna"                "Tenor Sans"                       
#> [1265] "Text Me One"                       "Texturina"                        
#> [1267] "Thasadith"                         "The Girl Next Door"               
#> [1269] "The Nautigal"                      "Tienne"                           
#> [1271] "Tillana"                           "Timmana"                          
#> [1273] "Tinos"                             "Titan One"                        
#> [1275] "Titillium Web"                     "Tomorrow"                         
#> [1277] "Tourney"                           "Trade Winds"                      
#> [1279] "Train One"                         "Trirong"                          
#> [1281] "Trispace"                          "Trocchi"                          
#> [1283] "Trochut"                           "Truculenta"                       
#> [1285] "Trykker"                           "Tulpen One"                       
#> [1287] "Turret Road"                       "Twinkle Star"                     
#> [1289] "Ubuntu"                            "Ubuntu Condensed"                 
#> [1291] "Ubuntu Mono"                       "Uchen"                            
#> [1293] "Ultra"                             "Uncial Antiqua"                   
#> [1295] "Underdog"                          "Unica One"                        
#> [1297] "UnifrakturCook"                    "UnifrakturMaguntia"               
#> [1299] "Unkempt"                           "Unlock"                           
#> [1301] "Unna"                              "Urbanist"                         
#> [1303] "VT323"                             "Vampiro One"                      
#> [1305] "Varela"                            "Varela Round"                     
#> [1307] "Varta"                             "Vast Shadow"                      
#> [1309] "Vesper Libre"                      "Viaoda Libre"                     
#> [1311] "Vibes"                             "Vibur"                            
#> [1313] "Vidaloka"                          "Viga"                             
#> [1315] "Voces"                             "Volkhov"                          
#> [1317] "Vollkorn"                          "Vollkorn SC"                      
#> [1319] "Voltaire"                          "Vujahday Script"                  
#> [1321] "Waiting for the Sunrise"           "Wallpoet"                         
#> [1323] "Walter Turncoat"                   "Warnes"                           
#> [1325] "Waterfall"                         "Wellfleet"                        
#> [1327] "Wendy One"                         "WindSong"                         
#> [1329] "Wire One"                          "Work Sans"                        
#> [1331] "Xanh Mono"                         "Yaldevi"                          
#> [1333] "Yanone Kaffeesatz"                 "Yantramanav"                      
#> [1335] "Yatra One"                         "Yellowtail"                       
#> [1337] "Yeon Sung"                         "Yeseva One"                       
#> [1339] "Yesteryear"                        "Yomogi"                           
#> [1341] "Yrsa"                              "Yuji Boku"                        
#> [1343] "Yuji Hentaigana Akari"             "Yuji Hentaigana Akebono"          
#> [1345] "Yuji Mai"                          "Yuji Syuku"                       
#> [1347] "Yusei Magic"                       "ZCOOL KuaiLe"                     
#> [1349] "ZCOOL QingKe HuangYou"             "ZCOOL XiaoWei"                    
#> [1351] "Zen Antique"                       "Zen Antique Soft"                 
#> [1353] "Zen Dots"                          "Zen Kaku Gothic Antique"          
#> [1355] "Zen Kaku Gothic New"               "Zen Kurenaido"                    
#> [1357] "Zen Loop"                          "Zen Maru Gothic"                  
#> [1359] "Zen Old Mincho"                    "Zen Tokyo Zoo"                    
#> [1361] "Zeyada"                            "Zhi Mang Xing"                    
#> [1363] "Zilla Slab"                        "Zilla Slab Highlight"             
#> 
list_fonts(family_only = FALSE, source = "builtin")
#>                            name         family       font weight
#> 1       liberationsans-700-bold LiberationSans       Bold    700
#> 2 liberationsans-700-bolditalic LiberationSans BoldItalic    700
#> 3     liberationsans-400-italic LiberationSans     Italic    400
#> 4    liberationsans-400-regular LiberationSans    Regular    400
#>                                                                                                   path
#> 1       /tmp/RtmptUvhZh/temp_libpath2597340b2dcf/plotthis/fonts/LiberationSans/LiberationSans-Bold.ttf
#> 2 /tmp/RtmptUvhZh/temp_libpath2597340b2dcf/plotthis/fonts/LiberationSans/LiberationSans-BoldItalic.ttf
#> 3     /tmp/RtmptUvhZh/temp_libpath2597340b2dcf/plotthis/fonts/LiberationSans/LiberationSans-Italic.ttf
#> 4    /tmp/RtmptUvhZh/temp_libpath2597340b2dcf/plotthis/fonts/LiberationSans/LiberationSans-Regular.ttf
#>    source
#> 1 builtin
#> 2 builtin
#> 3 builtin
#> 4 builtin
list_fonts(family_only = FALSE, source = "system")
#>                            name               family         font weight
#> 1                      d050000l             D050000L      Regular    400
#> 2             nimbusmonops-bold       Nimbus Mono PS         Bold    700
#> 3       nimbusmonops-bolditalic       Nimbus Mono PS  Bold Italic    700
#> 4           nimbusmonops-italic       Nimbus Mono PS       Italic    400
#> 5          nimbusmonops-regular       Nimbus Mono PS      Regular    400
#> 6              nimbusroman-bold         Nimbus Roman         Bold    700
#> 7        nimbusroman-bolditalic         Nimbus Roman  Bold Italic    700
#> 8            nimbusroman-italic         Nimbus Roman       Italic    400
#> 9           nimbusroman-regular         Nimbus Roman      Regular    400
#> 10              nimbussans-bold          Nimbus Sans         Bold    700
#> 11        nimbussans-bolditalic          Nimbus Sans  Bold Italic    700
#> 12            nimbussans-italic          Nimbus Sans       Italic    400
#> 13           nimbussans-regular          Nimbus Sans      Regular    400
#> 14        nimbussansnarrow-bold   Nimbus Sans Narrow         Bold    700
#> 15 nimbussansnarrow-boldoblique   Nimbus Sans Narrow Bold Oblique    700
#> 16     nimbussansnarrow-oblique   Nimbus Sans Narrow      Oblique    400
#> 17     nimbussansnarrow-regular   Nimbus Sans Narrow      Regular    400
#> 18            standardsymbolsps  Standard Symbols PS      Regular    400
#> 19            dejavumathtexgyre DejaVu Math TeX Gyre      Regular    400
#> 20        droidsansfallbackfull  Droid Sans Fallback      Regular    400
#> 21                   lato-black           Lato Black      Regular    400
#> 22             lato-blackitalic           Lato Black       Italic    400
#> 23                    lato-bold                 Lato         Bold    700
#> 24              lato-bolditalic                 Lato  Bold Italic    700
#> 25                lato-hairline        Lato Hairline      Regular    400
#> 26          lato-hairlineitalic        Lato Hairline       Italic    400
#> 27                   lato-heavy           Lato Heavy      Regular    400
#> 28             lato-heavyitalic           Lato Heavy       Italic    400
#> 29                  lato-italic                 Lato       Italic    400
#> 30                   lato-light           Lato Light      Regular    400
#> 31             lato-lightitalic           Lato Light       Italic    400
#> 32                  lato-medium          Lato Medium      Regular    400
#> 33            lato-mediumitalic          Lato Medium       Italic    400
#> 34                 lato-regular                 Lato      Regular    400
#> 35                lato-semibold        Lato Semibold      Regular    400
#> 36          lato-semibolditalic        Lato Semibold       Italic    400
#> 37                    lato-thin            Lato Thin      Regular    400
#> 38              lato-thinitalic            Lato Thin       Italic    400
#> 39          liberationmono-bold      Liberation Mono         Bold    700
#> 40    liberationmono-bolditalic      Liberation Mono  Bold Italic    700
#> 41        liberationmono-italic      Liberation Mono       Italic    400
#> 42       liberationmono-regular      Liberation Mono      Regular    400
#> 43          liberationsans-bold      Liberation Sans         Bold    700
#> 44    liberationsans-bolditalic      Liberation Sans  Bold Italic    700
#> 45        liberationsans-italic      Liberation Sans       Italic    400
#> 46       liberationsans-regular      Liberation Sans      Regular    400
#> 47         liberationserif-bold     Liberation Serif         Bold    700
#> 48   liberationserif-bolditalic     Liberation Serif  Bold Italic    700
#> 49       liberationserif-italic     Liberation Serif       Italic    400
#> 50      liberationserif-regular     Liberation Serif      Regular    400
#> 51               notocoloremoji     Noto Color Emoji      Regular    400
#> 52             notomono-regular            Noto Mono      Regular    400
#> 53            notosansmono-bold       Noto Sans Mono         Bold    700
#> 54         notosansmono-regular       Noto Sans Mono      Regular    400
#>                                                                     path source
#> 1                      /usr/share/fonts/opentype/urw-base35/D050000L.otf system
#> 2             /usr/share/fonts/opentype/urw-base35/NimbusMonoPS-Bold.otf system
#> 3       /usr/share/fonts/opentype/urw-base35/NimbusMonoPS-BoldItalic.otf system
#> 4           /usr/share/fonts/opentype/urw-base35/NimbusMonoPS-Italic.otf system
#> 5          /usr/share/fonts/opentype/urw-base35/NimbusMonoPS-Regular.otf system
#> 6              /usr/share/fonts/opentype/urw-base35/NimbusRoman-Bold.otf system
#> 7        /usr/share/fonts/opentype/urw-base35/NimbusRoman-BoldItalic.otf system
#> 8            /usr/share/fonts/opentype/urw-base35/NimbusRoman-Italic.otf system
#> 9           /usr/share/fonts/opentype/urw-base35/NimbusRoman-Regular.otf system
#> 10              /usr/share/fonts/opentype/urw-base35/NimbusSans-Bold.otf system
#> 11        /usr/share/fonts/opentype/urw-base35/NimbusSans-BoldItalic.otf system
#> 12            /usr/share/fonts/opentype/urw-base35/NimbusSans-Italic.otf system
#> 13           /usr/share/fonts/opentype/urw-base35/NimbusSans-Regular.otf system
#> 14        /usr/share/fonts/opentype/urw-base35/NimbusSansNarrow-Bold.otf system
#> 15 /usr/share/fonts/opentype/urw-base35/NimbusSansNarrow-BoldOblique.otf system
#> 16     /usr/share/fonts/opentype/urw-base35/NimbusSansNarrow-Oblique.otf system
#> 17     /usr/share/fonts/opentype/urw-base35/NimbusSansNarrow-Regular.otf system
#> 18            /usr/share/fonts/opentype/urw-base35/StandardSymbolsPS.otf system
#> 19                /usr/share/fonts/truetype/dejavu/DejaVuMathTeXGyre.ttf system
#> 20             /usr/share/fonts/truetype/droid/DroidSansFallbackFull.ttf system
#> 21                         /usr/share/fonts/truetype/lato/Lato-Black.ttf system
#> 22                   /usr/share/fonts/truetype/lato/Lato-BlackItalic.ttf system
#> 23                          /usr/share/fonts/truetype/lato/Lato-Bold.ttf system
#> 24                    /usr/share/fonts/truetype/lato/Lato-BoldItalic.ttf system
#> 25                      /usr/share/fonts/truetype/lato/Lato-Hairline.ttf system
#> 26                /usr/share/fonts/truetype/lato/Lato-HairlineItalic.ttf system
#> 27                         /usr/share/fonts/truetype/lato/Lato-Heavy.ttf system
#> 28                   /usr/share/fonts/truetype/lato/Lato-HeavyItalic.ttf system
#> 29                        /usr/share/fonts/truetype/lato/Lato-Italic.ttf system
#> 30                         /usr/share/fonts/truetype/lato/Lato-Light.ttf system
#> 31                   /usr/share/fonts/truetype/lato/Lato-LightItalic.ttf system
#> 32                        /usr/share/fonts/truetype/lato/Lato-Medium.ttf system
#> 33                  /usr/share/fonts/truetype/lato/Lato-MediumItalic.ttf system
#> 34                       /usr/share/fonts/truetype/lato/Lato-Regular.ttf system
#> 35                      /usr/share/fonts/truetype/lato/Lato-Semibold.ttf system
#> 36                /usr/share/fonts/truetype/lato/Lato-SemiboldItalic.ttf system
#> 37                          /usr/share/fonts/truetype/lato/Lato-Thin.ttf system
#> 38                    /usr/share/fonts/truetype/lato/Lato-ThinItalic.ttf system
#> 39          /usr/share/fonts/truetype/liberation/LiberationMono-Bold.ttf system
#> 40    /usr/share/fonts/truetype/liberation/LiberationMono-BoldItalic.ttf system
#> 41        /usr/share/fonts/truetype/liberation/LiberationMono-Italic.ttf system
#> 42       /usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf system
#> 43          /usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf system
#> 44    /usr/share/fonts/truetype/liberation/LiberationSans-BoldItalic.ttf system
#> 45        /usr/share/fonts/truetype/liberation/LiberationSans-Italic.ttf system
#> 46       /usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf system
#> 47         /usr/share/fonts/truetype/liberation/LiberationSerif-Bold.ttf system
#> 48   /usr/share/fonts/truetype/liberation/LiberationSerif-BoldItalic.ttf system
#> 49       /usr/share/fonts/truetype/liberation/LiberationSerif-Italic.ttf system
#> 50      /usr/share/fonts/truetype/liberation/LiberationSerif-Regular.ttf system
#> 51                     /usr/share/fonts/truetype/noto/NotoColorEmoji.ttf system
#> 52                   /usr/share/fonts/truetype/noto/NotoMono-Regular.ttf system
#> 53                  /usr/share/fonts/truetype/noto/NotoSansMono-Bold.ttf system
#> 54               /usr/share/fonts/truetype/noto/NotoSansMono-Regular.ttf system
list_fonts(family_only = FALSE, source = "google")
#>                                   name                            family
#> 1                              abeezee                           ABeeZee
#> 2                                 abel                              Abel
#> 3                         abhaya libre                      Abhaya Libre
#> 4                        abril fatface                     Abril Fatface
#> 5                             aclonica                          Aclonica
#> 6                                 acme                              Acme
#> 7                                actor                             Actor
#> 8                              adamina                           Adamina
#> 9                           advent pro                        Advent Pro
#> 10                     aguafina script                   Aguafina Script
#> 11                      akaya kanadaka                    Akaya Kanadaka
#> 12                    akaya telivigala                  Akaya Telivigala
#> 13                             akronim                           Akronim
#> 14                              aladin                            Aladin
#> 15                               alata                             Alata
#> 16                              alatsi                            Alatsi
#> 17                             aldrich                           Aldrich
#> 18                                alef                              Alef
#> 19                            alegreya                          Alegreya
#> 20                         alegreya sc                       Alegreya SC
#> 21                       alegreya sans                     Alegreya Sans
#> 22                    alegreya sans sc                  Alegreya Sans SC
#> 23                                aleo                              Aleo
#> 24                          alex brush                        Alex Brush
#> 25                       alfa slab one                     Alfa Slab One
#> 26                               alice                             Alice
#> 27                               alike                             Alike
#> 28                       alike angular                     Alike Angular
#> 29                               allan                             Allan
#> 30                             allerta                           Allerta
#> 31                     allerta stencil                   Allerta Stencil
#> 32                             allison                           Allison
#> 33                              allura                            Allura
#> 34                             almarai                           Almarai
#> 35                            almendra                          Almendra
#> 36                    almendra display                  Almendra Display
#> 37                         almendra sc                       Almendra SC
#> 38                         alumni sans                       Alumni Sans
#> 39                            amarante                          Amarante
#> 40                            amaranth                          Amaranth
#> 41                           amatic sc                         Amatic SC
#> 42                           amethysta                         Amethysta
#> 43                               amiko                             Amiko
#> 44                               amiri                             Amiri
#> 45                               amita                             Amita
#> 46                             anaheim                           Anaheim
#> 47                          andada pro                        Andada Pro
#> 48                              andika                            Andika
#> 49                    andika new basic                  Andika New Basic
#> 50                              angkor                            Angkor
#> 51            annie use your telescope          Annie Use Your Telescope
#> 52                       anonymous pro                     Anonymous Pro
#> 53                               antic                             Antic
#> 54                        antic didone                      Antic Didone
#> 55                          antic slab                        Antic Slab
#> 56                               anton                             Anton
#> 57                             antonio                           Antonio
#> 58                              arapey                            Arapey
#> 59                             arbutus                           Arbutus
#> 60                        arbutus slab                      Arbutus Slab
#> 61                 architects daughter               Architects Daughter
#> 62                             archivo                           Archivo
#> 63                       archivo black                     Archivo Black
#> 64                      archivo narrow                    Archivo Narrow
#> 65                     are you serious                   Are You Serious
#> 66                          aref ruqaa                        Aref Ruqaa
#> 67                       arima madurai                     Arima Madurai
#> 68                               arimo                             Arimo
#> 69                            arizonia                          Arizonia
#> 70                              armata                            Armata
#> 71                             arsenal                           Arsenal
#> 72                            artifika                          Artifika
#> 73                                arvo                              Arvo
#> 74                                arya                              Arya
#> 75                                asap                              Asap
#> 76                      asap condensed                    Asap Condensed
#> 77                                asar                              Asar
#> 78                               asset                             Asset
#> 79                           assistant                         Assistant
#> 80                             astloch                           Astloch
#> 81                                asul                              Asul
#> 82                              athiti                            Athiti
#> 83               atkinson hyperlegible             Atkinson Hyperlegible
#> 84                                atma                              Atma
#> 85                          atomic age                        Atomic Age
#> 86                              aubrey                            Aubrey
#> 87                           audiowide                         Audiowide
#> 88                          autour one                        Autour One
#> 89                             average                           Average
#> 90                        average sans                      Average Sans
#> 91                 averia gruesa libre               Averia Gruesa Libre
#> 92                        averia libre                      Averia Libre
#> 93                   averia sans libre                 Averia Sans Libre
#> 94                  averia serif libre                Averia Serif Libre
#> 95                         azeret mono                       Azeret Mono
#> 96                                b612                              B612
#> 97                           b612 mono                         B612 Mono
#> 98                          bad script                        Bad Script
#> 99                             bahiana                           Bahiana
#> 100                          bahianita                         Bahianita
#> 101                       bai jamjuree                      Bai Jamjuree
#> 102                         bakbak one                        Bakbak One
#> 103                             ballet                            Ballet
#> 104                            baloo 2                           Baloo 2
#> 105                       baloo bhai 2                      Baloo Bhai 2
#> 106                   baloo bhaijaan 2                  Baloo Bhaijaan 2
#> 107                     baloo bhaina 2                    Baloo Bhaina 2
#> 108                    baloo chettan 2                   Baloo Chettan 2
#> 109                         baloo da 2                        Baloo Da 2
#> 110                      baloo paaji 2                     Baloo Paaji 2
#> 111                      baloo tamma 2                     Baloo Tamma 2
#> 112                    baloo tammudu 2                   Baloo Tammudu 2
#> 113                     baloo thambi 2                    Baloo Thambi 2
#> 114                      balsamiq sans                     Balsamiq Sans
#> 115                          balthazar                         Balthazar
#> 116                            bangers                           Bangers
#> 117                             barlow                            Barlow
#> 118                   barlow condensed                  Barlow Condensed
#> 119              barlow semi condensed             Barlow Semi Condensed
#> 120                         barriecito                        Barriecito
#> 121                             barrio                            Barrio
#> 122                              basic                             Basic
#> 123                       baskervville                      Baskervville
#> 124                         battambang                        Battambang
#> 125                            baumans                           Baumans
#> 126                              bayon                             Bayon
#> 127                     be vietnam pro                    Be Vietnam Pro
#> 128                         bebas neue                        Bebas Neue
#> 129                           belgrano                          Belgrano
#> 130                          bellefair                         Bellefair
#> 131                            belleza                           Belleza
#> 132                            bellota                           Bellota
#> 133                       bellota text                      Bellota Text
#> 134                          benchnine                         BenchNine
#> 135                              benne                             Benne
#> 136                            bentham                           Bentham
#> 137                    berkshire swash                   Berkshire Swash
#> 138                             besley                            Besley
#> 139                         beth ellen                        Beth Ellen
#> 140                              bevan                             Bevan
#> 141               bhutuka expanded one              BhuTuka Expanded One
#> 142              big shoulders display             Big Shoulders Display
#> 143       big shoulders inline display      Big Shoulders Inline Display
#> 144          big shoulders inline text         Big Shoulders Inline Text
#> 145      big shoulders stencil display     Big Shoulders Stencil Display
#> 146         big shoulders stencil text        Big Shoulders Stencil Text
#> 147                 big shoulders text                Big Shoulders Text
#> 148                      bigelow rules                     Bigelow Rules
#> 149                        bigshot one                       Bigshot One
#> 150                              bilbo                             Bilbo
#> 151                   bilbo swash caps                  Bilbo Swash Caps
#> 152                           biorhyme                          BioRhyme
#> 153                  biorhyme expanded                 BioRhyme Expanded
#> 154                         birthstone                        Birthstone
#> 155                  birthstone bounce                 Birthstone Bounce
#> 156                            biryani                           Biryani
#> 157                             bitter                            Bitter
#> 158            black and white picture           Black And White Picture
#> 159                     black han sans                    Black Han Sans
#> 160                      black ops one                     Black Ops One
#> 161                            blinker                           Blinker
#> 162                        bodoni moda                       Bodoni Moda
#> 163                              bokor                             Bokor
#> 164                          bona nova                         Bona Nova
#> 165                             bonbon                            Bonbon
#> 166                     bonheur royale                    Bonheur Royale
#> 167                           boogaloo                          Boogaloo
#> 168                         bowlby one                        Bowlby One
#> 169                      bowlby one sc                     Bowlby One SC
#> 170                            brawler                           Brawler
#> 171                         bree serif                        Bree Serif
#> 172                       brygada 1918                      Brygada 1918
#> 173                     bubblegum sans                    Bubblegum Sans
#> 174                        bubbler one                       Bubbler One
#> 175                               buda                              Buda
#> 176                            buenard                           Buenard
#> 177                             bungee                            Bungee
#> 178                    bungee hairline                   Bungee Hairline
#> 179                      bungee inline                     Bungee Inline
#> 180                     bungee outline                    Bungee Outline
#> 181                       bungee shade                      Bungee Shade
#> 182                         butcherman                        Butcherman
#> 183                     butterfly kids                    Butterfly Kids
#> 184                              cabin                             Cabin
#> 185                    cabin condensed                   Cabin Condensed
#> 186                       cabin sketch                      Cabin Sketch
#> 187                    caesar dressing                   Caesar Dressing
#> 188                         cagliostro                        Cagliostro
#> 189                              cairo                             Cairo
#> 190                            caladea                           Caladea
#> 191                          calistoga                         Calistoga
#> 192                     calligraffitti                    Calligraffitti
#> 193                             cambay                            Cambay
#> 194                              cambo                             Cambo
#> 195                             candal                            Candal
#> 196                          cantarell                         Cantarell
#> 197                        cantata one                       Cantata One
#> 198                        cantora one                       Cantora One
#> 199                           capriola                          Capriola
#> 200                            caramel                           Caramel
#> 201                          carattere                         Carattere
#> 202                              cardo                             Cardo
#> 203                              carme                             Carme
#> 204                     carrois gothic                    Carrois Gothic
#> 205                  carrois gothic sc                 Carrois Gothic SC
#> 206                         carter one                        Carter One
#> 207                            castoro                           Castoro
#> 208                          catamaran                         Catamaran
#> 209                             caudex                            Caudex
#> 210                             caveat                            Caveat
#> 211                       caveat brush                      Caveat Brush
#> 212                 cedarville cursive                Cedarville Cursive
#> 213                        ceviche one                       Ceviche One
#> 214                       chakra petch                      Chakra Petch
#> 215                             changa                            Changa
#> 216                         changa one                        Changa One
#> 217                             chango                            Chango
#> 218                              charm                             Charm
#> 219                         charmonman                        Charmonman
#> 220                           chathura                          Chathura
#> 221                 chau philomene one                Chau Philomene One
#> 222                          chela one                         Chela One
#> 223                     chelsea market                    Chelsea Market
#> 224                             chenla                            Chenla
#> 225                            cherish                           Cherish
#> 226                  cherry cream soda                 Cherry Cream Soda
#> 227                       cherry swash                      Cherry Swash
#> 228                              chewy                             Chewy
#> 229                             chicle                            Chicle
#> 230                           chilanka                          Chilanka
#> 231                              chivo                             Chivo
#> 232                           chonburi                          Chonburi
#> 233                             cinzel                            Cinzel
#> 234                  cinzel decorative                 Cinzel Decorative
#> 235                     clicker script                    Clicker Script
#> 236                               coda                              Coda
#> 237                       coda caption                      Coda Caption
#> 238                           codystar                          Codystar
#> 239                              coiny                             Coiny
#> 240                              combo                             Combo
#> 241                          comfortaa                         Comfortaa
#> 242                          comforter                         Comforter
#> 243                    comforter brush                   Comforter Brush
#> 244                         comic neue                        Comic Neue
#> 245                        coming soon                       Coming Soon
#> 246                       commissioner                      Commissioner
#> 247                        concert one                       Concert One
#> 248                          condiment                         Condiment
#> 249                            content                           Content
#> 250                       contrail one                      Contrail One
#> 251                        convergence                       Convergence
#> 252                             cookie                            Cookie
#> 253                              copse                             Copse
#> 254                             corben                            Corben
#> 255                          corinthia                         Corinthia
#> 256                          cormorant                         Cormorant
#> 257                 cormorant garamond                Cormorant Garamond
#> 258                   cormorant infant                  Cormorant Infant
#> 259                       cormorant sc                      Cormorant SC
#> 260                  cormorant unicase                 Cormorant Unicase
#> 261                  cormorant upright                 Cormorant Upright
#> 262                          courgette                         Courgette
#> 263                      courier prime                     Courier Prime
#> 264                            cousine                           Cousine
#> 265                           coustard                          Coustard
#> 266              covered by your grace             Covered By Your Grace
#> 267                       crafty girls                      Crafty Girls
#> 268                          creepster                         Creepster
#> 269                        crete round                       Crete Round
#> 270                        crimson pro                       Crimson Pro
#> 271                      croissant one                     Croissant One
#> 272                            crushed                           Crushed
#> 273                             cuprum                            Cuprum
#> 274                          cute font                         Cute Font
#> 275                             cutive                            Cutive
#> 276                        cutive mono                       Cutive Mono
#> 277                            dm mono                           DM Mono
#> 278                            dm sans                           DM Sans
#> 279                   dm serif display                  DM Serif Display
#> 280                      dm serif text                     DM Serif Text
#> 281                             damion                            Damion
#> 282                     dancing script                    Dancing Script
#> 283                            dangrek                           Dangrek
#> 284                   darker grotesque                  Darker Grotesque
#> 285                        david libre                       David Libre
#> 286               dawning of a new day              Dawning of a New Day
#> 287                           days one                          Days One
#> 288                              dekko                             Dekko
#> 289                    dela gothic one                   Dela Gothic One
#> 290                             delius                            Delius
#> 291                  delius swash caps                 Delius Swash Caps
#> 292                     delius unicase                    Delius Unicase
#> 293                      della respira                     Della Respira
#> 294                           denk one                          Denk One
#> 295                         devonshire                        Devonshire
#> 296                           dhurjati                          Dhurjati
#> 297                      didact gothic                     Didact Gothic
#> 298                          diplomata                         Diplomata
#> 299                       diplomata sc                      Diplomata SC
#> 300                           do hyeon                          Do Hyeon
#> 301                              dokdo                             Dokdo
#> 302                             domine                            Domine
#> 303                        donegal one                       Donegal One
#> 304                             dongle                            Dongle
#> 305                         doppio one                        Doppio One
#> 306                              dorsa                             Dorsa
#> 307                              dosis                             Dosis
#> 308                        dotgothic16                       DotGothic16
#> 309                        dr sugiyama                       Dr Sugiyama
#> 310                          duru sans                         Duru Sans
#> 311                          dynalight                         Dynalight
#> 312                        eb garamond                       EB Garamond
#> 313                         eagle lake                        Eagle Lake
#> 314                     east sea dokdo                    East Sea Dokdo
#> 315                              eater                             Eater
#> 316                          economica                         Economica
#> 317                              eczar                             Eczar
#> 318                         el messiri                        El Messiri
#> 319                        electrolize                       Electrolize
#> 320                              elsie                             Elsie
#> 321                   elsie swash caps                  Elsie Swash Caps
#> 322                        emblema one                       Emblema One
#> 323                       emilys candy                      Emilys Candy
#> 324                        encode sans                       Encode Sans
#> 325              encode sans condensed             Encode Sans Condensed
#> 326               encode sans expanded              Encode Sans Expanded
#> 327                     encode sans sc                    Encode Sans SC
#> 328         encode sans semi condensed        Encode Sans Semi Condensed
#> 329          encode sans semi expanded         Encode Sans Semi Expanded
#> 330                         engagement                        Engagement
#> 331                          englebert                         Englebert
#> 332                          enriqueta                         Enriqueta
#> 333                            ephesis                           Ephesis
#> 334                           epilogue                          Epilogue
#> 335                          erica one                         Erica One
#> 336                            esteban                           Esteban
#> 337                            estonia                           Estonia
#> 338                    euphoria script                   Euphoria Script
#> 339                              ewert                             Ewert
#> 340                                exo                               Exo
#> 341                              exo 2                             Exo 2
#> 342                      expletus sans                     Expletus Sans
#> 343                            explora                           Explora
#> 344                           fahkwang                          Fahkwang
#> 345                       fanwood text                      Fanwood Text
#> 346                              farro                             Farro
#> 347                             farsan                            Farsan
#> 348                          fascinate                         Fascinate
#> 349                   fascinate inline                  Fascinate Inline
#> 350                         faster one                        Faster One
#> 351                           fasthand                          Fasthand
#> 352                          fauna one                         Fauna One
#> 353                           faustina                          Faustina
#> 354                           federant                          Federant
#> 355                             federo                            Federo
#> 356                             felipa                            Felipa
#> 357                              fenix                             Fenix
#> 358                            festive                           Festive
#> 359                       finger paint                      Finger Paint
#> 360                          fira code                         Fira Code
#> 361                          fira mono                         Fira Mono
#> 362                          fira sans                         Fira Sans
#> 363                fira sans condensed               Fira Sans Condensed
#> 364          fira sans extra condensed         Fira Sans Extra Condensed
#> 365                         fjalla one                        Fjalla One
#> 366                          fjord one                         Fjord One
#> 367                           flamenco                          Flamenco
#> 368                            flavors                           Flavors
#> 369                      fleur de leah                     Fleur De Leah
#> 370                         flow block                        Flow Block
#> 371                      flow circular                     Flow Circular
#> 372                       flow rounded                      Flow Rounded
#> 373                         fondamento                        Fondamento
#> 374                   fontdiner swanky                  Fontdiner Swanky
#> 375                              forum                             Forum
#> 376                       francois one                      Francois One
#> 377                   frank ruhl libre                  Frank Ruhl Libre
#> 378                           fraunces                          Fraunces
#> 379                       freckle face                      Freckle Face
#> 380               fredericka the great              Fredericka the Great
#> 381                            fredoka                           Fredoka
#> 382                        fredoka one                       Fredoka One
#> 383                           freehand                          Freehand
#> 384                             fresca                            Fresca
#> 385                            frijole                           Frijole
#> 386                            fruktur                           Fruktur
#> 387                          fugaz one                         Fugaz One
#> 388                            fuggles                           Fuggles
#> 389                      fuzzy bubbles                     Fuzzy Bubbles
#> 390                          gfs didot                         GFS Didot
#> 391                    gfs neohellenic                   GFS Neohellenic
#> 392                           gabriela                          Gabriela
#> 393                              gaegu                             Gaegu
#> 394                             gafata                            Gafata
#> 395                             galada                            Galada
#> 396                           galdeano                          Galdeano
#> 397                            galindo                           Galindo
#> 398                       gamja flower                      Gamja Flower
#> 399                           gayathri                          Gayathri
#> 400                            gelasio                           Gelasio
#> 401                       gemunu libre                      Gemunu Libre
#> 402                              genos                             Genos
#> 403                      gentium basic                     Gentium Basic
#> 404                 gentium book basic                Gentium Book Basic
#> 405                                geo                               Geo
#> 406                            georama                           Georama
#> 407                            geostar                           Geostar
#> 408                       geostar fill                      Geostar Fill
#> 409                       germania one                      Germania One
#> 410                       gideon roman                      Gideon Roman
#> 411                             gidugu                            Gidugu
#> 412                      gilda display                     Gilda Display
#> 413                           girassol                          Girassol
#> 414                     give you glory                    Give You Glory
#> 415                      glass antiqua                     Glass Antiqua
#> 416                             glegoo                            Glegoo
#> 417                  gloria hallelujah                 Gloria Hallelujah
#> 418                              glory                             Glory
#> 419                             gluten                            Gluten
#> 420                         goblin one                        Goblin One
#> 421                         gochi hand                        Gochi Hand
#> 422                            goldman                           Goldman
#> 423                           gorditas                          Gorditas
#> 424                          gothic a1                         Gothic A1
#> 425                               gotu                              Gotu
#> 426              goudy bookletter 1911             Goudy Bookletter 1911
#> 427                       gowun batang                      Gowun Batang
#> 428                        gowun dodum                       Gowun Dodum
#> 429                           graduate                          Graduate
#> 430                        grand hotel                       Grand Hotel
#> 431                       grandstander                      Grandstander
#> 432                       gravitas one                      Gravitas One
#> 433                        great vibes                       Great Vibes
#> 434                     grechen fuemen                    Grechen Fuemen
#> 435                             grenze                            Grenze
#> 436                     grenze gotisch                    Grenze Gotisch
#> 437                            grey qo                           Grey Qo
#> 438                             griffy                            Griffy
#> 439                             gruppo                            Gruppo
#> 440                              gudea                             Gudea
#> 441                               gugi                              Gugi
#> 442                             gupter                            Gupter
#> 443                           gurajada                          Gurajada
#> 444                          gwendolyn                         Gwendolyn
#> 445                             habibi                            Habibi
#> 446                     hachi maru pop                    Hachi Maru Pop
#> 447                            hahmlet                           Hahmlet
#> 448                             halant                            Halant
#> 449                    hammersmith one                   Hammersmith One
#> 450                            hanalei                           Hanalei
#> 451                       hanalei fill                      Hanalei Fill
#> 452                            handlee                           Handlee
#> 453                            hanuman                           Hanuman
#> 454                       happy monkey                      Happy Monkey
#> 455                          harmattan                         Harmattan
#> 456                       headland one                      Headland One
#> 457                              heebo                             Heebo
#> 458                        henny penny                       Henny Penny
#> 459                         hepta slab                        Hepta Slab
#> 460               herr von muellerhoff              Herr Von Muellerhoff
#> 461                          hi melody                         Hi Melody
#> 462                        hina mincho                       Hina Mincho
#> 463                               hind                              Hind
#> 464                        hind guntur                       Hind Guntur
#> 465                       hind madurai                      Hind Madurai
#> 466                      hind siliguri                     Hind Siliguri
#> 467                      hind vadodara                     Hind Vadodara
#> 468                    holtwood one sc                   Holtwood One SC
#> 469                     homemade apple                    Homemade Apple
#> 470                           homenaje                          Homenaje
#> 471                           hubballi                          Hubballi
#> 472                          hurricane                         Hurricane
#> 473                      ibm plex mono                     IBM Plex Mono
#> 474                      ibm plex sans                     IBM Plex Sans
#> 475               ibm plex sans arabic              IBM Plex Sans Arabic
#> 476            ibm plex sans condensed           IBM Plex Sans Condensed
#> 477           ibm plex sans devanagari          IBM Plex Sans Devanagari
#> 478               ibm plex sans hebrew              IBM Plex Sans Hebrew
#> 479                   ibm plex sans kr                  IBM Plex Sans KR
#> 480                 ibm plex sans thai                IBM Plex Sans Thai
#> 481          ibm plex sans thai looped         IBM Plex Sans Thai Looped
#> 482                     ibm plex serif                    IBM Plex Serif
#> 483                    im fell dw pica                   IM Fell DW Pica
#> 484                 im fell dw pica sc                IM Fell DW Pica SC
#> 485                im fell double pica               IM Fell Double Pica
#> 486             im fell double pica sc            IM Fell Double Pica SC
#> 487                    im fell english                   IM Fell English
#> 488                 im fell english sc                IM Fell English SC
#> 489               im fell french canon              IM Fell French Canon
#> 490            im fell french canon sc           IM Fell French Canon SC
#> 491               im fell great primer              IM Fell Great Primer
#> 492            im fell great primer sc           IM Fell Great Primer SC
#> 493                   ibarra real nova                  Ibarra Real Nova
#> 494                            iceberg                           Iceberg
#> 495                            iceland                           Iceland
#> 496                              imbue                             Imbue
#> 497                    imperial script                   Imperial Script
#> 498                            imprima                           Imprima
#> 499                        inconsolata                       Inconsolata
#> 500                              inder                             Inder
#> 501                       indie flower                      Indie Flower
#> 502                              inika                             Inika
#> 503                     inknut antiqua                    Inknut Antiqua
#> 504                         inria sans                        Inria Sans
#> 505                        inria serif                       Inria Serif
#> 506                        inspiration                       Inspiration
#> 507                              inter                             Inter
#> 508                       irish grover                      Irish Grover
#> 509                     island moments                    Island Moments
#> 510                          istok web                         Istok Web
#> 511                           italiana                          Italiana
#> 512                          italianno                         Italianno
#> 513                               itim                              Itim
#> 514                   jacques francois                  Jacques Francois
#> 515            jacques francois shadow           Jacques Francois Shadow
#> 516                              jaldi                             Jaldi
#> 517                     jetbrains mono                    JetBrains Mono
#> 518                     jim nightshade                    Jim Nightshade
#> 519                         jockey one                        Jockey One
#> 520                       jolly lodger                      Jolly Lodger
#> 521                           jomhuria                          Jomhuria
#> 522                          jomolhari                         Jomolhari
#> 523                       josefin sans                      Josefin Sans
#> 524                       josefin slab                      Josefin Slab
#> 525                               jost                              Jost
#> 526                           joti one                          Joti One
#> 527                                jua                               Jua
#> 528                             judson                            Judson
#> 529                              julee                             Julee
#> 530                    julius sans one                   Julius Sans One
#> 531                              junge                             Junge
#> 532                               jura                              Jura
#> 533                  just another hand                 Just Another Hand
#> 534            just me again down here           Just Me Again Down Here
#> 535                                k2d                               K2D
#> 536                              kadwa                             Kadwa
#> 537                       kaisei decol                      Kaisei Decol
#> 538                   kaisei harunoumi                  Kaisei HarunoUmi
#> 539                        kaisei opti                       Kaisei Opti
#> 540                     kaisei tokumin                    Kaisei Tokumin
#> 541                              kalam                             Kalam
#> 542                            kameron                           Kameron
#> 543                              kanit                             Kanit
#> 544                          kantumruy                         Kantumruy
#> 545                          karantina                         Karantina
#> 546                              karla                             Karla
#> 547                              karma                             Karma
#> 548                            katibeh                           Katibeh
#> 549                     kaushan script                    Kaushan Script
#> 550                          kavivanar                         Kavivanar
#> 551                             kavoon                            Kavoon
#> 552                         kdam thmor                        Kdam Thmor
#> 553                         keania one                        Keania One
#> 554                         kelly slab                        Kelly Slab
#> 555                              kenia                             Kenia
#> 556                              khand                             Khand
#> 557                              khmer                             Khmer
#> 558                              khula                             Khula
#> 559                              kings                             Kings
#> 560                     kirang haerang                    Kirang Haerang
#> 561                           kite one                          Kite One
#> 562                          kiwi maru                         Kiwi Maru
#> 563                           klee one                          Klee One
#> 564                            knewave                           Knewave
#> 565                               koho                              KoHo
#> 566                          kodchasan                         Kodchasan
#> 567                     koh santepheap                    Koh Santepheap
#> 568                       kolker brush                      Kolker Brush
#> 569                             kosugi                            Kosugi
#> 570                        kosugi maru                       Kosugi Maru
#> 571                          kotta one                         Kotta One
#> 572                             koulen                            Koulen
#> 573                             kranky                            Kranky
#> 574                              kreon                             Kreon
#> 575                             kristi                            Kristi
#> 576                          krona one                         Krona One
#> 577                               krub                              Krub
#> 578                              kufam                             Kufam
#> 579                         kulim park                        Kulim Park
#> 580                          kumar one                         Kumar One
#> 581                  kumar one outline                 Kumar One Outline
#> 582                         kumbh sans                        Kumbh Sans
#> 583                             kurale                            Kurale
#> 584                    la belle aurore                   La Belle Aurore
#> 585                            lacquer                           Lacquer
#> 586                              laila                             Laila
#> 587                        lakki reddy                       Lakki Reddy
#> 588                            lalezar                           Lalezar
#> 589                           lancelot                          Lancelot
#> 590                             langar                            Langar
#> 591                             lateef                            Lateef
#> 592                               lato                              Lato
#> 593                      league gothic                     League Gothic
#> 594                      league script                     League Script
#> 595                     league spartan                    League Spartan
#> 596                       leckerli one                      Leckerli One
#> 597                             ledger                            Ledger
#> 598                             lekton                            Lekton
#> 599                              lemon                             Lemon
#> 600                           lemonada                          Lemonada
#> 601                             lexend                            Lexend
#> 602                        lexend deca                       Lexend Deca
#> 603                         lexend exa                        Lexend Exa
#> 604                        lexend giga                       Lexend Giga
#> 605                        lexend mega                       Lexend Mega
#> 606                        lexend peta                       Lexend Peta
#> 607                        lexend tera                       Lexend Tera
#> 608                       lexend zetta                      Lexend Zetta
#> 609                  libre barcode 128                 Libre Barcode 128
#> 610             libre barcode 128 text            Libre Barcode 128 Text
#> 611                   libre barcode 39                  Libre Barcode 39
#> 612          libre barcode 39 extended         Libre Barcode 39 Extended
#> 613     libre barcode 39 extended text    Libre Barcode 39 Extended Text
#> 614              libre barcode 39 text             Libre Barcode 39 Text
#> 615           libre barcode ean13 text          Libre Barcode EAN13 Text
#> 616                  libre baskerville                 Libre Baskerville
#> 617               libre caslon display              Libre Caslon Display
#> 618                  libre caslon text                 Libre Caslon Text
#> 619                     libre franklin                    Libre Franklin
#> 620                           licorice                          Licorice
#> 621                        life savers                       Life Savers
#> 622                         lilita one                        Lilita One
#> 623                    lily script one                   Lily Script One
#> 624                          limelight                         Limelight
#> 625                        linden hill                       Linden Hill
#> 626                           literata                          Literata
#> 627                   liu jian mao cao                  Liu Jian Mao Cao
#> 628                             livvic                            Livvic
#> 629                            lobster                           Lobster
#> 630                        lobster two                       Lobster Two
#> 631                   londrina outline                  Londrina Outline
#> 632                    londrina shadow                   Londrina Shadow
#> 633                    londrina sketch                   Londrina Sketch
#> 634                     londrina solid                    Londrina Solid
#> 635                          long cang                         Long Cang
#> 636                               lora                              Lora
#> 637                         love light                        Love Light
#> 638              love ya like a sister             Love Ya Like A Sister
#> 639                  loved by the king                 Loved by the King
#> 640                     lovers quarrel                    Lovers Quarrel
#> 641                       luckiest guy                      Luckiest Guy
#> 642                           lusitana                          Lusitana
#> 643                            lustria                           Lustria
#> 644                    luxurious roman                   Luxurious Roman
#> 645                   luxurious script                  Luxurious Script
#> 646                           m plus 1                          M PLUS 1
#> 647                      m plus 1 code                     M PLUS 1 Code
#> 648                          m plus 1p                         M PLUS 1p
#> 649                           m plus 2                          M PLUS 2
#> 650                  m plus code latin                 M PLUS Code Latin
#> 651                  m plus rounded 1c                 M PLUS Rounded 1c
#> 652                      ma shan zheng                     Ma Shan Zheng
#> 653                            macondo                           Macondo
#> 654                 macondo swash caps                Macondo Swash Caps
#> 655                               mada                              Mada
#> 656                              magra                             Magra
#> 657                      maiden orange                     Maiden Orange
#> 658                            maitree                           Maitree
#> 659                 major mono display                Major Mono Display
#> 660                               mako                              Mako
#> 661                               mali                              Mali
#> 662                           mallanna                          Mallanna
#> 663                            mandali                           Mandali
#> 664                            manjari                           Manjari
#> 665                            manrope                           Manrope
#> 666                           mansalva                          Mansalva
#> 667                            manuale                           Manuale
#> 668                          marcellus                         Marcellus
#> 669                       marcellus sc                      Marcellus SC
#> 670                       marck script                      Marck Script
#> 671                          margarine                         Margarine
#> 672                       markazi text                      Markazi Text
#> 673                          marko one                         Marko One
#> 674                           marmelad                          Marmelad
#> 675                             martel                            Martel
#> 676                        martel sans                       Martel Sans
#> 677                             marvel                            Marvel
#> 678                               mate                              Mate
#> 679                            mate sc                           Mate SC
#> 680                          maven pro                         Maven Pro
#> 681                            mclaren                           McLaren
#> 682                          mea culpa                         Mea Culpa
#> 683                             meddon                            Meddon
#> 684                      medievalsharp                     MedievalSharp
#> 685                         medula one                        Medula One
#> 686                       meera inimai                      Meera Inimai
#> 687                             megrim                            Megrim
#> 688                        meie script                       Meie Script
#> 689                        meow script                       Meow Script
#> 690                           merienda                          Merienda
#> 691                       merienda one                      Merienda One
#> 692                       merriweather                      Merriweather
#> 693                  merriweather sans                 Merriweather Sans
#> 694                              metal                             Metal
#> 695                        metal mania                       Metal Mania
#> 696                       metamorphous                      Metamorphous
#> 697                        metrophobic                       Metrophobic
#> 698                           michroma                          Michroma
#> 699                            milonga                           Milonga
#> 700                          miltonian                         Miltonian
#> 701                   miltonian tattoo                  Miltonian Tattoo
#> 702                               mina                              Mina
#> 703                            miniver                           Miniver
#> 704                       miriam libre                      Miriam Libre
#> 705                              mirza                             Mirza
#> 706                     miss fajardose                    Miss Fajardose
#> 707                               mitr                              Mitr
#> 708                     mochiy pop one                    Mochiy Pop One
#> 709                   mochiy pop p one                  Mochiy Pop P One
#> 710                              modak                             Modak
#> 711                     modern antiqua                    Modern Antiqua
#> 712                              mogra                             Mogra
#> 713                             mohave                            Mohave
#> 714                            molengo                           Molengo
#> 715                              molle                             Molle
#> 716                              monda                             Monda
#> 717                           monofett                          Monofett
#> 718                            monoton                           Monoton
#> 719               monsieur la doulaise              Monsieur La Doulaise
#> 720                            montaga                           Montaga
#> 721                       montagu slab                      Montagu Slab
#> 722                         montecarlo                        MonteCarlo
#> 723                             montez                            Montez
#> 724                         montserrat                        Montserrat
#> 725              montserrat alternates             Montserrat Alternates
#> 726               montserrat subrayada              Montserrat Subrayada
#> 727                        moo lah lah                       Moo Lah Lah
#> 728                         moon dance                        Moon Dance
#> 729                               moul                              Moul
#> 730                           moulpali                          Moulpali
#> 731             mountains of christmas            Mountains of Christmas
#> 732                      mouse memoirs                     Mouse Memoirs
#> 733                         mr bedfort                        Mr Bedfort
#> 734                           mr dafoe                          Mr Dafoe
#> 735                     mr de haviland                    Mr De Haviland
#> 736                mrs saint delafield               Mrs Saint Delafield
#> 737                      mrs sheppards                     Mrs Sheppards
#> 738                              mukta                             Mukta
#> 739                        mukta mahee                       Mukta Mahee
#> 740                        mukta malar                       Mukta Malar
#> 741                        mukta vaani                       Mukta Vaani
#> 742                             mulish                            Mulish
#> 743                            murecho                           Murecho
#> 744                       museomoderno                      MuseoModerno
#> 745                      mystery quest                     Mystery Quest
#> 746                                ntr                               NTR
#> 747                 nanum brush script                Nanum Brush Script
#> 748                       nanum gothic                      Nanum Gothic
#> 749                nanum gothic coding               Nanum Gothic Coding
#> 750                     nanum myeongjo                    Nanum Myeongjo
#> 751                   nanum pen script                  Nanum Pen Script
#> 752                        neonderthaw                       Neonderthaw
#> 753                          nerko one                         Nerko One
#> 754                             neucha                            Neucha
#> 755                             neuton                            Neuton
#> 756                         new rocker                        New Rocker
#> 757                        new tegomin                       New Tegomin
#> 758                         news cycle                        News Cycle
#> 759                         newsreader                        Newsreader
#> 760                            niconne                           Niconne
#> 761                            niramit                           Niramit
#> 762                          nixie one                         Nixie One
#> 763                             nobile                            Nobile
#> 764                             nokora                            Nokora
#> 765                            norican                           Norican
#> 766                            nosifer                           Nosifer
#> 767                            notable                           Notable
#> 768               nothing you could do              Nothing You Could Do
#> 769                       noticia text                      Noticia Text
#> 770                   noto kufi arabic                  Noto Kufi Arabic
#> 771                         noto music                        Noto Music
#> 772                  noto naskh arabic                 Noto Naskh Arabic
#> 773                 noto nastaliq urdu                Noto Nastaliq Urdu
#> 774                  noto rashi hebrew                 Noto Rashi Hebrew
#> 775                          noto sans                         Noto Sans
#> 776                    noto sans adlam                   Noto Sans Adlam
#> 777           noto sans adlam unjoined          Noto Sans Adlam Unjoined
#> 778    noto sans anatolian hieroglyphs   Noto Sans Anatolian Hieroglyphs
#> 779                   noto sans arabic                  Noto Sans Arabic
#> 780                 noto sans armenian                Noto Sans Armenian
#> 781                  noto sans avestan                 Noto Sans Avestan
#> 782                 noto sans balinese                Noto Sans Balinese
#> 783                    noto sans bamum                   Noto Sans Bamum
#> 784                noto sans bassa vah               Noto Sans Bassa Vah
#> 785                    noto sans batak                   Noto Sans Batak
#> 786                  noto sans bengali                 Noto Sans Bengali
#> 787                noto sans bhaiksuki               Noto Sans Bhaiksuki
#> 788                   noto sans brahmi                  Noto Sans Brahmi
#> 789                 noto sans buginese                Noto Sans Buginese
#> 790                    noto sans buhid                   Noto Sans Buhid
#> 791      noto sans canadian aboriginal     Noto Sans Canadian Aboriginal
#> 792                   noto sans carian                  Noto Sans Carian
#> 793       noto sans caucasian albanian      Noto Sans Caucasian Albanian
#> 794                   noto sans chakma                  Noto Sans Chakma
#> 795                     noto sans cham                    Noto Sans Cham
#> 796                 noto sans cherokee                Noto Sans Cherokee
#> 797                   noto sans coptic                  Noto Sans Coptic
#> 798                noto sans cuneiform               Noto Sans Cuneiform
#> 799                  noto sans cypriot                 Noto Sans Cypriot
#> 800                  noto sans deseret                 Noto Sans Deseret
#> 801               noto sans devanagari              Noto Sans Devanagari
#> 802                  noto sans display                 Noto Sans Display
#> 803                 noto sans duployan                Noto Sans Duployan
#> 804     noto sans egyptian hieroglyphs    Noto Sans Egyptian Hieroglyphs
#> 805                  noto sans elbasan                 Noto Sans Elbasan
#> 806                  noto sans elymaic                 Noto Sans Elymaic
#> 807                 noto sans georgian                Noto Sans Georgian
#> 808               noto sans glagolitic              Noto Sans Glagolitic
#> 809                   noto sans gothic                  Noto Sans Gothic
#> 810                  noto sans grantha                 Noto Sans Grantha
#> 811                 noto sans gujarati                Noto Sans Gujarati
#> 812            noto sans gunjala gondi           Noto Sans Gunjala Gondi
#> 813                 noto sans gurmukhi                Noto Sans Gurmukhi
#> 814                       noto sans hk                      Noto Sans HK
#> 815          noto sans hanifi rohingya         Noto Sans Hanifi Rohingya
#> 816                  noto sans hanunoo                 Noto Sans Hanunoo
#> 817                   noto sans hatran                  Noto Sans Hatran
#> 818                   noto sans hebrew                  Noto Sans Hebrew
#> 819         noto sans imperial aramaic        Noto Sans Imperial Aramaic
#> 820      noto sans indic siyaq numbers     Noto Sans Indic Siyaq Numbers
#> 821    noto sans inscriptional pahlavi   Noto Sans Inscriptional Pahlavi
#> 822   noto sans inscriptional parthian  Noto Sans Inscriptional Parthian
#> 823                       noto sans jp                      Noto Sans JP
#> 824                 noto sans javanese                Noto Sans Javanese
#> 825                       noto sans kr                      Noto Sans KR
#> 826                   noto sans kaithi                  Noto Sans Kaithi
#> 827                  noto sans kannada                 Noto Sans Kannada
#> 828                 noto sans kayah li                Noto Sans Kayah Li
#> 829               noto sans kharoshthi              Noto Sans Kharoshthi
#> 830                    noto sans khmer                   Noto Sans Khmer
#> 831                   noto sans khojki                  Noto Sans Khojki
#> 832                noto sans khudawadi               Noto Sans Khudawadi
#> 833                      noto sans lao                     Noto Sans Lao
#> 834                   noto sans lepcha                  Noto Sans Lepcha
#> 835                    noto sans limbu                   Noto Sans Limbu
#> 836                 noto sans linear a                Noto Sans Linear A
#> 837                 noto sans linear b                Noto Sans Linear B
#> 838                     noto sans lisu                    Noto Sans Lisu
#> 839                   noto sans lycian                  Noto Sans Lycian
#> 840                   noto sans lydian                  Noto Sans Lydian
#> 841                 noto sans mahajani                Noto Sans Mahajani
#> 842                noto sans malayalam               Noto Sans Malayalam
#> 843                  noto sans mandaic                 Noto Sans Mandaic
#> 844               noto sans manichaean              Noto Sans Manichaean
#> 845                  noto sans marchen                 Noto Sans Marchen
#> 846            noto sans masaram gondi           Noto Sans Masaram Gondi
#> 847                     noto sans math                    Noto Sans Math
#> 848           noto sans mayan numerals          Noto Sans Mayan Numerals
#> 849              noto sans medefaidrin             Noto Sans Medefaidrin
#> 850             noto sans meetei mayek            Noto Sans Meetei Mayek
#> 851                 noto sans meroitic                Noto Sans Meroitic
#> 852                     noto sans miao                    Noto Sans Miao
#> 853                     noto sans modi                    Noto Sans Modi
#> 854                noto sans mongolian               Noto Sans Mongolian
#> 855                     noto sans mono                    Noto Sans Mono
#> 856                      noto sans mro                     Noto Sans Mro
#> 857                  noto sans multani                 Noto Sans Multani
#> 858                  noto sans myanmar                 Noto Sans Myanmar
#> 859                     noto sans n ko                    Noto Sans N Ko
#> 860                noto sans nabataean               Noto Sans Nabataean
#> 861              noto sans new tai lue             Noto Sans New Tai Lue
#> 862                     noto sans newa                    Noto Sans Newa
#> 863                    noto sans nushu                   Noto Sans Nushu
#> 864                    noto sans ogham                   Noto Sans Ogham
#> 865                 noto sans ol chiki                Noto Sans Ol Chiki
#> 866            noto sans old hungarian           Noto Sans Old Hungarian
#> 867               noto sans old italic              Noto Sans Old Italic
#> 868        noto sans old north arabian       Noto Sans Old North Arabian
#> 869               noto sans old permic              Noto Sans Old Permic
#> 870              noto sans old persian             Noto Sans Old Persian
#> 871              noto sans old sogdian             Noto Sans Old Sogdian
#> 872        noto sans old south arabian       Noto Sans Old South Arabian
#> 873               noto sans old turkic              Noto Sans Old Turkic
#> 874                    noto sans oriya                   Noto Sans Oriya
#> 875                    noto sans osage                   Noto Sans Osage
#> 876                  noto sans osmanya                 Noto Sans Osmanya
#> 877             noto sans pahawh hmong            Noto Sans Pahawh Hmong
#> 878                noto sans palmyrene               Noto Sans Palmyrene
#> 879              noto sans pau cin hau             Noto Sans Pau Cin Hau
#> 880                 noto sans phags pa                Noto Sans Phags Pa
#> 881               noto sans phoenician              Noto Sans Phoenician
#> 882          noto sans psalter pahlavi         Noto Sans Psalter Pahlavi
#> 883                   noto sans rejang                  Noto Sans Rejang
#> 884                    noto sans runic                   Noto Sans Runic
#> 885                       noto sans sc                      Noto Sans SC
#> 886                noto sans samaritan               Noto Sans Samaritan
#> 887               noto sans saurashtra              Noto Sans Saurashtra
#> 888                  noto sans sharada                 Noto Sans Sharada
#> 889                  noto sans shavian                 Noto Sans Shavian
#> 890                  noto sans siddham                 Noto Sans Siddham
#> 891                  noto sans sinhala                 Noto Sans Sinhala
#> 892                  noto sans sogdian                 Noto Sans Sogdian
#> 893             noto sans sora sompeng            Noto Sans Sora Sompeng
#> 894                  noto sans soyombo                 Noto Sans Soyombo
#> 895                noto sans sundanese               Noto Sans Sundanese
#> 896             noto sans syloti nagri            Noto Sans Syloti Nagri
#> 897                  noto sans symbols                 Noto Sans Symbols
#> 898                noto sans symbols 2               Noto Sans Symbols 2
#> 899                   noto sans syriac                  Noto Sans Syriac
#> 900                       noto sans tc                      Noto Sans TC
#> 901                  noto sans tagalog                 Noto Sans Tagalog
#> 902                 noto sans tagbanwa                Noto Sans Tagbanwa
#> 903                   noto sans tai le                  Noto Sans Tai Le
#> 904                 noto sans tai tham                Noto Sans Tai Tham
#> 905                 noto sans tai viet                Noto Sans Tai Viet
#> 906                    noto sans takri                   Noto Sans Takri
#> 907                    noto sans tamil                   Noto Sans Tamil
#> 908         noto sans tamil supplement        Noto Sans Tamil Supplement
#> 909                   noto sans telugu                  Noto Sans Telugu
#> 910                   noto sans thaana                  Noto Sans Thaana
#> 911                     noto sans thai                    Noto Sans Thai
#> 912              noto sans thai looped             Noto Sans Thai Looped
#> 913                 noto sans tifinagh                Noto Sans Tifinagh
#> 914                  noto sans tirhuta                 Noto Sans Tirhuta
#> 915                 noto sans ugaritic                Noto Sans Ugaritic
#> 916                      noto sans vai                     Noto Sans Vai
#> 917                   noto sans wancho                  Noto Sans Wancho
#> 918              noto sans warang citi             Noto Sans Warang Citi
#> 919                       noto sans yi                      Noto Sans Yi
#> 920         noto sans zanabazar square        Noto Sans Zanabazar Square
#> 921                         noto serif                        Noto Serif
#> 922                    noto serif ahom                   Noto Serif Ahom
#> 923                noto serif armenian               Noto Serif Armenian
#> 924                noto serif balinese               Noto Serif Balinese
#> 925                 noto serif bengali                Noto Serif Bengali
#> 926              noto serif devanagari             Noto Serif Devanagari
#> 927                 noto serif display                Noto Serif Display
#> 928                   noto serif dogra                  Noto Serif Dogra
#> 929                noto serif ethiopic               Noto Serif Ethiopic
#> 930                noto serif georgian               Noto Serif Georgian
#> 931                 noto serif grantha                Noto Serif Grantha
#> 932                noto serif gujarati               Noto Serif Gujarati
#> 933                noto serif gurmukhi               Noto Serif Gurmukhi
#> 934                  noto serif hebrew                 Noto Serif Hebrew
#> 935                      noto serif jp                     Noto Serif JP
#> 936                      noto serif kr                     Noto Serif KR
#> 937                 noto serif kannada                Noto Serif Kannada
#> 938                   noto serif khmer                  Noto Serif Khmer
#> 939                     noto serif lao                    Noto Serif Lao
#> 940               noto serif malayalam              Noto Serif Malayalam
#> 941                 noto serif myanmar                Noto Serif Myanmar
#> 942  noto serif nyiakeng puachue hmong Noto Serif Nyiakeng Puachue Hmong
#> 943                      noto serif sc                     Noto Serif SC
#> 944                 noto serif sinhala                Noto Serif Sinhala
#> 945                      noto serif tc                     Noto Serif TC
#> 946                   noto serif tamil                  Noto Serif Tamil
#> 947                  noto serif tangut                 Noto Serif Tangut
#> 948                  noto serif telugu                 Noto Serif Telugu
#> 949                    noto serif thai                   Noto Serif Thai
#> 950                 noto serif tibetan                Noto Serif Tibetan
#> 951                  noto serif yezidi                 Noto Serif Yezidi
#> 952             noto traditional nushu            Noto Traditional Nushu
#> 953                           nova cut                          Nova Cut
#> 954                          nova flat                         Nova Flat
#> 955                          nova mono                         Nova Mono
#> 956                          nova oval                         Nova Oval
#> 957                         nova round                        Nova Round
#> 958                        nova script                       Nova Script
#> 959                          nova slim                         Nova Slim
#> 960                        nova square                       Nova Square
#> 961                             numans                            Numans
#> 962                             nunito                            Nunito
#> 963                        nunito sans                       Nunito Sans
#> 964                        odibee sans                       Odibee Sans
#> 965                     odor mean chey                    Odor Mean Chey
#> 966                            offside                           Offside
#> 967                                 oi                                Oi
#> 968                    old standard tt                   Old Standard TT
#> 969                          oldenburg                         Oldenburg
#> 970                                ole                               Ole
#> 971                        oleo script                       Oleo Script
#> 972             oleo script swash caps            Oleo Script Swash Caps
#> 973                          oooh baby                         Oooh Baby
#> 974                          open sans                         Open Sans
#> 975                        oranienbaum                       Oranienbaum
#> 976                           orbitron                          Orbitron
#> 977                            oregano                           Oregano
#> 978                        orelega one                       Orelega One
#> 979                            orienta                           Orienta
#> 980                    original surfer                   Original Surfer
#> 981                             oswald                            Oswald
#> 982                     otomanopee one                    Otomanopee One
#> 983                             outfit                            Outfit
#> 984                   over the rainbow                  Over the Rainbow
#> 985                           overlock                          Overlock
#> 986                        overlock sc                       Overlock SC
#> 987                           overpass                          Overpass
#> 988                      overpass mono                     Overpass Mono
#> 989                                ovo                               Ovo
#> 990                            oxanium                           Oxanium
#> 991                             oxygen                            Oxygen
#> 992                        oxygen mono                       Oxygen Mono
#> 993                            pt mono                           PT Mono
#> 994                            pt sans                           PT Sans
#> 995                    pt sans caption                   PT Sans Caption
#> 996                     pt sans narrow                    PT Sans Narrow
#> 997                           pt serif                          PT Serif
#> 998                   pt serif caption                  PT Serif Caption
#> 999                           pacifico                          Pacifico
#> 1000                            padauk                            Padauk
#> 1001                         palanquin                         Palanquin
#> 1002                    palanquin dark                    Palanquin Dark
#> 1003                    palette mosaic                    Palette Mosaic
#> 1004                          pangolin                          Pangolin
#> 1005                           paprika                           Paprika
#> 1006                        parisienne                        Parisienne
#> 1007                       passero one                       Passero One
#> 1008                       passion one                       Passion One
#> 1009                 passions conflict                 Passions Conflict
#> 1010                pathway gothic one                Pathway Gothic One
#> 1011                      patrick hand                      Patrick Hand
#> 1012                   patrick hand sc                   Patrick Hand SC
#> 1013                           pattaya                           Pattaya
#> 1014                         patua one                         Patua One
#> 1015                           pavanam                           Pavanam
#> 1016                       paytone one                       Paytone One
#> 1017                           peddana                           Peddana
#> 1018                           peralta                           Peralta
#> 1019                  permanent marker                  Permanent Marker
#> 1020                          petemoss                          Petemoss
#> 1021               petit formal script               Petit Formal Script
#> 1022                           petrona                           Petrona
#> 1023                       philosopher                       Philosopher
#> 1024                         piazzolla                         Piazzolla
#> 1025                            piedra                            Piedra
#> 1026                     pinyon script                     Pinyon Script
#> 1027                        pirata one                        Pirata One
#> 1028                           plaster                           Plaster
#> 1029                              play                              Play
#> 1030                          playball                          Playball
#> 1031                  playfair display                  Playfair Display
#> 1032               playfair display sc               Playfair Display SC
#> 1033                           podkova                           Podkova
#> 1034                        poiret one                        Poiret One
#> 1035                        poller one                        Poller One
#> 1036                              poly                              Poly
#> 1037                          pompiere                          Pompiere
#> 1038                      pontano sans                      Pontano Sans
#> 1039                        poor story                        Poor Story
#> 1040                           poppins                           Poppins
#> 1041                  port lligat sans                  Port Lligat Sans
#> 1042                  port lligat slab                  Port Lligat Slab
#> 1043                         potta one                         Potta One
#> 1044                    pragati narrow                    Pragati Narrow
#> 1045                            praise                            Praise
#> 1046                             prata                             Prata
#> 1047                       preahvihear                       Preahvihear
#> 1048                    press start 2p                    Press Start 2P
#> 1049                             pridi                             Pridi
#> 1050                    princess sofia                    Princess Sofia
#> 1051                          prociono                          Prociono
#> 1052                            prompt                            Prompt
#> 1053                        prosto one                        Prosto One
#> 1054                       proza libre                       Proza Libre
#> 1055                       public sans                       Public Sans
#> 1056                      puppies play                      Puppies Play
#> 1057                           puritan                           Puritan
#> 1058                      purple purse                      Purple Purse
#> 1059                            qahiri                            Qahiri
#> 1060                            quando                            Quando
#> 1061                          quantico                          Quantico
#> 1062                      quattrocento                      Quattrocento
#> 1063                 quattrocento sans                 Quattrocento Sans
#> 1064                         questrial                         Questrial
#> 1065                         quicksand                         Quicksand
#> 1066                    quintessential                    Quintessential
#> 1067                           qwigley                           Qwigley
#> 1068                   qwitcher grypen                   Qwitcher Grypen
#> 1069                   racing sans one                   Racing Sans One
#> 1070                            radley                            Radley
#> 1071                          rajdhani                          Rajdhani
#> 1072                            rakkas                            Rakkas
#> 1073                           raleway                           Raleway
#> 1074                      raleway dots                      Raleway Dots
#> 1075                        ramabhadra                        Ramabhadra
#> 1076                          ramaraja                          Ramaraja
#> 1077                            rambla                            Rambla
#> 1078                      rammetto one                      Rammetto One
#> 1079                       rampart one                       Rampart One
#> 1080                          ranchers                          Ranchers
#> 1081                            rancho                            Rancho
#> 1082                             ranga                             Ranga
#> 1083                              rasa                              Rasa
#> 1084                         rationale                         Rationale
#> 1085                      ravi prakash                      Ravi Prakash
#> 1086                        readex pro                        Readex Pro
#> 1087                         recursive                         Recursive
#> 1088                   red hat display                   Red Hat Display
#> 1089                      red hat mono                      Red Hat Mono
#> 1090                      red hat text                      Red Hat Text
#> 1091                          red rose                          Red Rose
#> 1092                          redacted                          Redacted
#> 1093                   redacted script                   Redacted Script
#> 1094                         redressed                         Redressed
#> 1095                         reem kufi                         Reem Kufi
#> 1096                     reenie beanie                     Reenie Beanie
#> 1097                        reggae one                        Reggae One
#> 1098                           revalia                           Revalia
#> 1099                     rhodium libre                     Rhodium Libre
#> 1100                            ribeye                            Ribeye
#> 1101                     ribeye marrow                     Ribeye Marrow
#> 1102                         righteous                         Righteous
#> 1103                            risque                            Risque
#> 1104                         road rage                         Road Rage
#> 1105                            roboto                            Roboto
#> 1106                  roboto condensed                  Roboto Condensed
#> 1107                       roboto mono                       Roboto Mono
#> 1108                      roboto serif                      Roboto Serif
#> 1109                       roboto slab                       Roboto Slab
#> 1110                         rochester                         Rochester
#> 1111                           rock 3d                           Rock 3D
#> 1112                         rock salt                         Rock Salt
#> 1113                     rocknroll one                     RocknRoll One
#> 1114                           rokkitt                           Rokkitt
#> 1115                         romanesco                         Romanesco
#> 1116                         ropa sans                         Ropa Sans
#> 1117                           rosario                           Rosario
#> 1118                          rosarivo                          Rosarivo
#> 1119                      rouge script                      Rouge Script
#> 1120                           rowdies                           Rowdies
#> 1121                         rozha one                         Rozha One
#> 1122                             rubik                             Rubik
#> 1123                     rubik beastly                     Rubik Beastly
#> 1124                    rubik mono one                    Rubik Mono One
#> 1125                              ruda                              Ruda
#> 1126                            rufina                            Rufina
#> 1127                       ruge boogie                       Ruge Boogie
#> 1128                            ruluko                            Ruluko
#> 1129                        rum raisin                        Rum Raisin
#> 1130                    ruslan display                    Ruslan Display
#> 1131                         russo one                         Russo One
#> 1132                            ruthie                            Ruthie
#> 1133                               rye                               Rye
#> 1134                     stix two text                     STIX Two Text
#> 1135                        sacramento                        Sacramento
#> 1136                           sahitya                           Sahitya
#> 1137                              sail                              Sail
#> 1138                             saira                             Saira
#> 1139                   saira condensed                   Saira Condensed
#> 1140             saira extra condensed             Saira Extra Condensed
#> 1141              saira semi condensed              Saira Semi Condensed
#> 1142                 saira stencil one                 Saira Stencil One
#> 1143                             salsa                             Salsa
#> 1144                           sanchez                           Sanchez
#> 1145                          sancreek                          Sancreek
#> 1146                           sansita                           Sansita
#> 1147                   sansita swashed                   Sansita Swashed
#> 1148                           sarabun                           Sarabun
#> 1149                            sarala                            Sarala
#> 1150                            sarina                            Sarina
#> 1151                          sarpanch                          Sarpanch
#> 1152                       sassy frass                       Sassy Frass
#> 1153                           satisfy                           Satisfy
#> 1154                   sawarabi gothic                   Sawarabi Gothic
#> 1155                   sawarabi mincho                   Sawarabi Mincho
#> 1156                             scada                             Scada
#> 1157                  scheherazade new                  Scheherazade New
#> 1158                        schoolbell                        Schoolbell
#> 1159                         scope one                         Scope One
#> 1160                    seaweed script                    Seaweed Script
#> 1161                       secular one                       Secular One
#> 1162                      sedgwick ave                      Sedgwick Ave
#> 1163              sedgwick ave display              Sedgwick Ave Display
#> 1164                               sen                               Sen
#> 1165                         sevillana                         Sevillana
#> 1166                       seymour one                       Seymour One
#> 1167                shadows into light                Shadows Into Light
#> 1168            shadows into light two            Shadows Into Light Two
#> 1169                          shalimar                          Shalimar
#> 1170                            shanti                            Shanti
#> 1171                             share                             Share
#> 1172                        share tech                        Share Tech
#> 1173                   share tech mono                   Share Tech Mono
#> 1174                  shippori antique                  Shippori Antique
#> 1175               shippori antique b1               Shippori Antique B1
#> 1176                   shippori mincho                   Shippori Mincho
#> 1177                shippori mincho b1                Shippori Mincho B1
#> 1178                           shizuru                           Shizuru
#> 1179                         shojumaru                         Shojumaru
#> 1180                       short stack                       Short Stack
#> 1181                         shrikhand                         Shrikhand
#> 1182                          siemreap                          Siemreap
#> 1183                        sigmar one                        Sigmar One
#> 1184                           signika                           Signika
#> 1185                  signika negative                  Signika Negative
#> 1186                         simonetta                         Simonetta
#> 1187                        single day                        Single Day
#> 1188                           sintony                           Sintony
#> 1189                     sirin stencil                     Sirin Stencil
#> 1190                          six caps                          Six Caps
#> 1191                           skranji                           Skranji
#> 1192                        slabo 13px                        Slabo 13px
#> 1193                        slabo 27px                        Slabo 27px
#> 1194                           slackey                           Slackey
#> 1195                            smokum                            Smokum
#> 1196                            smooch                            Smooch
#> 1197                       smooch sans                       Smooch Sans
#> 1198                            smythe                            Smythe
#> 1199                           sniglet                           Sniglet
#> 1200                           snippet                           Snippet
#> 1201                     snowburst one                     Snowburst One
#> 1202                        sofadi one                        Sofadi One
#> 1203                             sofia                             Sofia
#> 1204                            solway                            Solway
#> 1205                        song myung                        Song Myung
#> 1206                        sonsie one                        Sonsie One
#> 1207                              sora                              Sora
#> 1208                  sorts mill goudy                  Sorts Mill Goudy
#> 1209                   source code pro                   Source Code Pro
#> 1210                     source sans 3                     Source Sans 3
#> 1211                   source sans pro                   Source Sans Pro
#> 1212                    source serif 4                    Source Serif 4
#> 1213                  source serif pro                  Source Serif Pro
#> 1214                     space grotesk                     Space Grotesk
#> 1215                        space mono                        Space Mono
#> 1216                           spartan                           Spartan
#> 1217                     special elite                     Special Elite
#> 1218                          spectral                          Spectral
#> 1219                       spectral sc                       Spectral SC
#> 1220                        spicy rice                        Spicy Rice
#> 1221                         spinnaker                         Spinnaker
#> 1222                            spirax                            Spirax
#> 1223                       spline sans                       Spline Sans
#> 1224                        squada one                        Squada One
#> 1225              sree krushnadevaraya              Sree Krushnadevaraya
#> 1226                          sriracha                          Sriracha
#> 1227                          srisakdi                          Srisakdi
#> 1228                       staatliches                       Staatliches
#> 1229                         stalemate                         Stalemate
#> 1230                     stalinist one                     Stalinist One
#> 1231                   stardos stencil                   Stardos Stencil
#> 1232                             stick                             Stick
#> 1233                    stick no bills                    Stick No Bills
#> 1234             stint ultra condensed             Stint Ultra Condensed
#> 1235              stint ultra expanded              Stint Ultra Expanded
#> 1236                             stoke                             Stoke
#> 1237                            strait                            Strait
#> 1238                      style script                      Style Script
#> 1239                           stylish                           Stylish
#> 1240               sue ellen francisco               Sue Ellen Francisco
#> 1241                          suez one                          Suez One
#> 1242                     sulphur point                     Sulphur Point
#> 1243                            sumana                            Sumana
#> 1244                         sunflower                         Sunflower
#> 1245                         sunshiney                         Sunshiney
#> 1246                  supermercado one                  Supermercado One
#> 1247                              sura                              Sura
#> 1248                           suranna                           Suranna
#> 1249                         suravaram                         Suravaram
#> 1250                       suwannaphum                       Suwannaphum
#> 1251                swanky and moo moo                Swanky and Moo Moo
#> 1252                         syncopate                         Syncopate
#> 1253                              syne                              Syne
#> 1254                         syne mono                         Syne Mono
#> 1255                      syne tactile                      Syne Tactile
#> 1256                           tajawal                           Tajawal
#> 1257                         tangerine                         Tangerine
#> 1258                            taprom                            Taprom
#> 1259                             tauri                             Tauri
#> 1260                           taviraj                           Taviraj
#> 1261                              teko                              Teko
#> 1262                             telex                             Telex
#> 1263                tenali ramakrishna                Tenali Ramakrishna
#> 1264                        tenor sans                        Tenor Sans
#> 1265                       text me one                       Text Me One
#> 1266                         texturina                         Texturina
#> 1267                         thasadith                         Thasadith
#> 1268                the girl next door                The Girl Next Door
#> 1269                      the nautigal                      The Nautigal
#> 1270                            tienne                            Tienne
#> 1271                           tillana                           Tillana
#> 1272                           timmana                           Timmana
#> 1273                             tinos                             Tinos
#> 1274                         titan one                         Titan One
#> 1275                     titillium web                     Titillium Web
#> 1276                          tomorrow                          Tomorrow
#> 1277                           tourney                           Tourney
#> 1278                       trade winds                       Trade Winds
#> 1279                         train one                         Train One
#> 1280                           trirong                           Trirong
#> 1281                          trispace                          Trispace
#> 1282                           trocchi                           Trocchi
#> 1283                           trochut                           Trochut
#> 1284                        truculenta                        Truculenta
#> 1285                           trykker                           Trykker
#> 1286                        tulpen one                        Tulpen One
#> 1287                       turret road                       Turret Road
#> 1288                      twinkle star                      Twinkle Star
#> 1289                            ubuntu                            Ubuntu
#> 1290                  ubuntu condensed                  Ubuntu Condensed
#> 1291                       ubuntu mono                       Ubuntu Mono
#> 1292                             uchen                             Uchen
#> 1293                             ultra                             Ultra
#> 1294                    uncial antiqua                    Uncial Antiqua
#> 1295                          underdog                          Underdog
#> 1296                         unica one                         Unica One
#> 1297                    unifrakturcook                    UnifrakturCook
#> 1298                unifrakturmaguntia                UnifrakturMaguntia
#> 1299                           unkempt                           Unkempt
#> 1300                            unlock                            Unlock
#> 1301                              unna                              Unna
#> 1302                          urbanist                          Urbanist
#> 1303                             vt323                             VT323
#> 1304                       vampiro one                       Vampiro One
#> 1305                            varela                            Varela
#> 1306                      varela round                      Varela Round
#> 1307                             varta                             Varta
#> 1308                       vast shadow                       Vast Shadow
#> 1309                      vesper libre                      Vesper Libre
#> 1310                      viaoda libre                      Viaoda Libre
#> 1311                             vibes                             Vibes
#> 1312                             vibur                             Vibur
#> 1313                          vidaloka                          Vidaloka
#> 1314                              viga                              Viga
#> 1315                             voces                             Voces
#> 1316                           volkhov                           Volkhov
#> 1317                          vollkorn                          Vollkorn
#> 1318                       vollkorn sc                       Vollkorn SC
#> 1319                          voltaire                          Voltaire
#> 1320                   vujahday script                   Vujahday Script
#> 1321           waiting for the sunrise           Waiting for the Sunrise
#> 1322                          wallpoet                          Wallpoet
#> 1323                   walter turncoat                   Walter Turncoat
#> 1324                            warnes                            Warnes
#> 1325                         waterfall                         Waterfall
#> 1326                         wellfleet                         Wellfleet
#> 1327                         wendy one                         Wendy One
#> 1328                          windsong                          WindSong
#> 1329                          wire one                          Wire One
#> 1330                         work sans                         Work Sans
#> 1331                         xanh mono                         Xanh Mono
#> 1332                           yaldevi                           Yaldevi
#> 1333                 yanone kaffeesatz                 Yanone Kaffeesatz
#> 1334                       yantramanav                       Yantramanav
#> 1335                         yatra one                         Yatra One
#> 1336                        yellowtail                        Yellowtail
#> 1337                         yeon sung                         Yeon Sung
#> 1338                        yeseva one                        Yeseva One
#> 1339                        yesteryear                        Yesteryear
#> 1340                            yomogi                            Yomogi
#> 1341                              yrsa                              Yrsa
#> 1342                         yuji boku                         Yuji Boku
#> 1343             yuji hentaigana akari             Yuji Hentaigana Akari
#> 1344           yuji hentaigana akebono           Yuji Hentaigana Akebono
#> 1345                          yuji mai                          Yuji Mai
#> 1346                        yuji syuku                        Yuji Syuku
#> 1347                       yusei magic                       Yusei Magic
#> 1348                      zcool kuaile                      ZCOOL KuaiLe
#> 1349             zcool qingke huangyou             ZCOOL QingKe HuangYou
#> 1350                     zcool xiaowei                     ZCOOL XiaoWei
#> 1351                       zen antique                       Zen Antique
#> 1352                  zen antique soft                  Zen Antique Soft
#> 1353                          zen dots                          Zen Dots
#> 1354           zen kaku gothic antique           Zen Kaku Gothic Antique
#> 1355               zen kaku gothic new               Zen Kaku Gothic New
#> 1356                     zen kurenaido                     Zen Kurenaido
#> 1357                          zen loop                          Zen Loop
#> 1358                   zen maru gothic                   Zen Maru Gothic
#> 1359                    zen old mincho                    Zen Old Mincho
#> 1360                     zen tokyo zoo                     Zen Tokyo Zoo
#> 1361                            zeyada                            Zeyada
#> 1362                     zhi mang xing                     Zhi Mang Xing
#> 1363                        zilla slab                        Zilla Slab
#> 1364              zilla slab highlight              Zilla Slab Highlight
#>                                   font weight path source
#> 1                              ABeeZee    400 <NA> google
#> 2                                 Abel    400 <NA> google
#> 3                         Abhaya Libre    400 <NA> google
#> 4                        Abril Fatface    400 <NA> google
#> 5                             Aclonica    400 <NA> google
#> 6                                 Acme    400 <NA> google
#> 7                                Actor    400 <NA> google
#> 8                              Adamina    400 <NA> google
#> 9                           Advent Pro    400 <NA> google
#> 10                     Aguafina Script    400 <NA> google
#> 11                      Akaya Kanadaka    400 <NA> google
#> 12                    Akaya Telivigala    400 <NA> google
#> 13                             Akronim    400 <NA> google
#> 14                              Aladin    400 <NA> google
#> 15                               Alata    400 <NA> google
#> 16                              Alatsi    400 <NA> google
#> 17                             Aldrich    400 <NA> google
#> 18                                Alef    400 <NA> google
#> 19                            Alegreya    400 <NA> google
#> 20                         Alegreya SC    400 <NA> google
#> 21                       Alegreya Sans    400 <NA> google
#> 22                    Alegreya Sans SC    400 <NA> google
#> 23                                Aleo    400 <NA> google
#> 24                          Alex Brush    400 <NA> google
#> 25                       Alfa Slab One    400 <NA> google
#> 26                               Alice    400 <NA> google
#> 27                               Alike    400 <NA> google
#> 28                       Alike Angular    400 <NA> google
#> 29                               Allan    400 <NA> google
#> 30                             Allerta    400 <NA> google
#> 31                     Allerta Stencil    400 <NA> google
#> 32                             Allison    400 <NA> google
#> 33                              Allura    400 <NA> google
#> 34                             Almarai    400 <NA> google
#> 35                            Almendra    400 <NA> google
#> 36                    Almendra Display    400 <NA> google
#> 37                         Almendra SC    400 <NA> google
#> 38                         Alumni Sans    400 <NA> google
#> 39                            Amarante    400 <NA> google
#> 40                            Amaranth    400 <NA> google
#> 41                           Amatic SC    400 <NA> google
#> 42                           Amethysta    400 <NA> google
#> 43                               Amiko    400 <NA> google
#> 44                               Amiri    400 <NA> google
#> 45                               Amita    400 <NA> google
#> 46                             Anaheim    400 <NA> google
#> 47                          Andada Pro    400 <NA> google
#> 48                              Andika    400 <NA> google
#> 49                    Andika New Basic    400 <NA> google
#> 50                              Angkor    400 <NA> google
#> 51            Annie Use Your Telescope    400 <NA> google
#> 52                       Anonymous Pro    400 <NA> google
#> 53                               Antic    400 <NA> google
#> 54                        Antic Didone    400 <NA> google
#> 55                          Antic Slab    400 <NA> google
#> 56                               Anton    400 <NA> google
#> 57                             Antonio    400 <NA> google
#> 58                              Arapey    400 <NA> google
#> 59                             Arbutus    400 <NA> google
#> 60                        Arbutus Slab    400 <NA> google
#> 61                 Architects Daughter    400 <NA> google
#> 62                             Archivo    400 <NA> google
#> 63                       Archivo Black    400 <NA> google
#> 64                      Archivo Narrow    400 <NA> google
#> 65                     Are You Serious    400 <NA> google
#> 66                          Aref Ruqaa    400 <NA> google
#> 67                       Arima Madurai    400 <NA> google
#> 68                               Arimo    400 <NA> google
#> 69                            Arizonia    400 <NA> google
#> 70                              Armata    400 <NA> google
#> 71                             Arsenal    400 <NA> google
#> 72                            Artifika    400 <NA> google
#> 73                                Arvo    400 <NA> google
#> 74                                Arya    400 <NA> google
#> 75                                Asap    400 <NA> google
#> 76                      Asap Condensed    400 <NA> google
#> 77                                Asar    400 <NA> google
#> 78                               Asset    400 <NA> google
#> 79                           Assistant    400 <NA> google
#> 80                             Astloch    400 <NA> google
#> 81                                Asul    400 <NA> google
#> 82                              Athiti    400 <NA> google
#> 83               Atkinson Hyperlegible    400 <NA> google
#> 84                                Atma    400 <NA> google
#> 85                          Atomic Age    400 <NA> google
#> 86                              Aubrey    400 <NA> google
#> 87                           Audiowide    400 <NA> google
#> 88                          Autour One    400 <NA> google
#> 89                             Average    400 <NA> google
#> 90                        Average Sans    400 <NA> google
#> 91                 Averia Gruesa Libre    400 <NA> google
#> 92                        Averia Libre    400 <NA> google
#> 93                   Averia Sans Libre    400 <NA> google
#> 94                  Averia Serif Libre    400 <NA> google
#> 95                         Azeret Mono    400 <NA> google
#> 96                                B612    400 <NA> google
#> 97                           B612 Mono    400 <NA> google
#> 98                          Bad Script    400 <NA> google
#> 99                             Bahiana    400 <NA> google
#> 100                          Bahianita    400 <NA> google
#> 101                       Bai Jamjuree    400 <NA> google
#> 102                         Bakbak One    400 <NA> google
#> 103                             Ballet    400 <NA> google
#> 104                            Baloo 2    400 <NA> google
#> 105                       Baloo Bhai 2    400 <NA> google
#> 106                   Baloo Bhaijaan 2    400 <NA> google
#> 107                     Baloo Bhaina 2    400 <NA> google
#> 108                    Baloo Chettan 2    400 <NA> google
#> 109                         Baloo Da 2    400 <NA> google
#> 110                      Baloo Paaji 2    400 <NA> google
#> 111                      Baloo Tamma 2    400 <NA> google
#> 112                    Baloo Tammudu 2    400 <NA> google
#> 113                     Baloo Thambi 2    400 <NA> google
#> 114                      Balsamiq Sans    400 <NA> google
#> 115                          Balthazar    400 <NA> google
#> 116                            Bangers    400 <NA> google
#> 117                             Barlow    400 <NA> google
#> 118                   Barlow Condensed    400 <NA> google
#> 119              Barlow Semi Condensed    400 <NA> google
#> 120                         Barriecito    400 <NA> google
#> 121                             Barrio    400 <NA> google
#> 122                              Basic    400 <NA> google
#> 123                       Baskervville    400 <NA> google
#> 124                         Battambang    400 <NA> google
#> 125                            Baumans    400 <NA> google
#> 126                              Bayon    400 <NA> google
#> 127                     Be Vietnam Pro    400 <NA> google
#> 128                         Bebas Neue    400 <NA> google
#> 129                           Belgrano    400 <NA> google
#> 130                          Bellefair    400 <NA> google
#> 131                            Belleza    400 <NA> google
#> 132                            Bellota    400 <NA> google
#> 133                       Bellota Text    400 <NA> google
#> 134                          BenchNine    400 <NA> google
#> 135                              Benne    400 <NA> google
#> 136                            Bentham    400 <NA> google
#> 137                    Berkshire Swash    400 <NA> google
#> 138                             Besley    400 <NA> google
#> 139                         Beth Ellen    400 <NA> google
#> 140                              Bevan    400 <NA> google
#> 141               BhuTuka Expanded One    400 <NA> google
#> 142              Big Shoulders Display    400 <NA> google
#> 143       Big Shoulders Inline Display    400 <NA> google
#> 144          Big Shoulders Inline Text    400 <NA> google
#> 145      Big Shoulders Stencil Display    400 <NA> google
#> 146         Big Shoulders Stencil Text    400 <NA> google
#> 147                 Big Shoulders Text    400 <NA> google
#> 148                      Bigelow Rules    400 <NA> google
#> 149                        Bigshot One    400 <NA> google
#> 150                              Bilbo    400 <NA> google
#> 151                   Bilbo Swash Caps    400 <NA> google
#> 152                           BioRhyme    400 <NA> google
#> 153                  BioRhyme Expanded    400 <NA> google
#> 154                         Birthstone    400 <NA> google
#> 155                  Birthstone Bounce    400 <NA> google
#> 156                            Biryani    400 <NA> google
#> 157                             Bitter    400 <NA> google
#> 158            Black And White Picture    400 <NA> google
#> 159                     Black Han Sans    400 <NA> google
#> 160                      Black Ops One    400 <NA> google
#> 161                            Blinker    400 <NA> google
#> 162                        Bodoni Moda    400 <NA> google
#> 163                              Bokor    400 <NA> google
#> 164                          Bona Nova    400 <NA> google
#> 165                             Bonbon    400 <NA> google
#> 166                     Bonheur Royale    400 <NA> google
#> 167                           Boogaloo    400 <NA> google
#> 168                         Bowlby One    400 <NA> google
#> 169                      Bowlby One SC    400 <NA> google
#> 170                            Brawler    400 <NA> google
#> 171                         Bree Serif    400 <NA> google
#> 172                       Brygada 1918    400 <NA> google
#> 173                     Bubblegum Sans    400 <NA> google
#> 174                        Bubbler One    400 <NA> google
#> 175                               Buda    400 <NA> google
#> 176                            Buenard    400 <NA> google
#> 177                             Bungee    400 <NA> google
#> 178                    Bungee Hairline    400 <NA> google
#> 179                      Bungee Inline    400 <NA> google
#> 180                     Bungee Outline    400 <NA> google
#> 181                       Bungee Shade    400 <NA> google
#> 182                         Butcherman    400 <NA> google
#> 183                     Butterfly Kids    400 <NA> google
#> 184                              Cabin    400 <NA> google
#> 185                    Cabin Condensed    400 <NA> google
#> 186                       Cabin Sketch    400 <NA> google
#> 187                    Caesar Dressing    400 <NA> google
#> 188                         Cagliostro    400 <NA> google
#> 189                              Cairo    400 <NA> google
#> 190                            Caladea    400 <NA> google
#> 191                          Calistoga    400 <NA> google
#> 192                     Calligraffitti    400 <NA> google
#> 193                             Cambay    400 <NA> google
#> 194                              Cambo    400 <NA> google
#> 195                             Candal    400 <NA> google
#> 196                          Cantarell    400 <NA> google
#> 197                        Cantata One    400 <NA> google
#> 198                        Cantora One    400 <NA> google
#> 199                           Capriola    400 <NA> google
#> 200                            Caramel    400 <NA> google
#> 201                          Carattere    400 <NA> google
#> 202                              Cardo    400 <NA> google
#> 203                              Carme    400 <NA> google
#> 204                     Carrois Gothic    400 <NA> google
#> 205                  Carrois Gothic SC    400 <NA> google
#> 206                         Carter One    400 <NA> google
#> 207                            Castoro    400 <NA> google
#> 208                          Catamaran    400 <NA> google
#> 209                             Caudex    400 <NA> google
#> 210                             Caveat    400 <NA> google
#> 211                       Caveat Brush    400 <NA> google
#> 212                 Cedarville Cursive    400 <NA> google
#> 213                        Ceviche One    400 <NA> google
#> 214                       Chakra Petch    400 <NA> google
#> 215                             Changa    400 <NA> google
#> 216                         Changa One    400 <NA> google
#> 217                             Chango    400 <NA> google
#> 218                              Charm    400 <NA> google
#> 219                         Charmonman    400 <NA> google
#> 220                           Chathura    400 <NA> google
#> 221                 Chau Philomene One    400 <NA> google
#> 222                          Chela One    400 <NA> google
#> 223                     Chelsea Market    400 <NA> google
#> 224                             Chenla    400 <NA> google
#> 225                            Cherish    400 <NA> google
#> 226                  Cherry Cream Soda    400 <NA> google
#> 227                       Cherry Swash    400 <NA> google
#> 228                              Chewy    400 <NA> google
#> 229                             Chicle    400 <NA> google
#> 230                           Chilanka    400 <NA> google
#> 231                              Chivo    400 <NA> google
#> 232                           Chonburi    400 <NA> google
#> 233                             Cinzel    400 <NA> google
#> 234                  Cinzel Decorative    400 <NA> google
#> 235                     Clicker Script    400 <NA> google
#> 236                               Coda    400 <NA> google
#> 237                       Coda Caption    400 <NA> google
#> 238                           Codystar    400 <NA> google
#> 239                              Coiny    400 <NA> google
#> 240                              Combo    400 <NA> google
#> 241                          Comfortaa    400 <NA> google
#> 242                          Comforter    400 <NA> google
#> 243                    Comforter Brush    400 <NA> google
#> 244                         Comic Neue    400 <NA> google
#> 245                        Coming Soon    400 <NA> google
#> 246                       Commissioner    400 <NA> google
#> 247                        Concert One    400 <NA> google
#> 248                          Condiment    400 <NA> google
#> 249                            Content    400 <NA> google
#> 250                       Contrail One    400 <NA> google
#> 251                        Convergence    400 <NA> google
#> 252                             Cookie    400 <NA> google
#> 253                              Copse    400 <NA> google
#> 254                             Corben    400 <NA> google
#> 255                          Corinthia    400 <NA> google
#> 256                          Cormorant    400 <NA> google
#> 257                 Cormorant Garamond    400 <NA> google
#> 258                   Cormorant Infant    400 <NA> google
#> 259                       Cormorant SC    400 <NA> google
#> 260                  Cormorant Unicase    400 <NA> google
#> 261                  Cormorant Upright    400 <NA> google
#> 262                          Courgette    400 <NA> google
#> 263                      Courier Prime    400 <NA> google
#> 264                            Cousine    400 <NA> google
#> 265                           Coustard    400 <NA> google
#> 266              Covered By Your Grace    400 <NA> google
#> 267                       Crafty Girls    400 <NA> google
#> 268                          Creepster    400 <NA> google
#> 269                        Crete Round    400 <NA> google
#> 270                        Crimson Pro    400 <NA> google
#> 271                      Croissant One    400 <NA> google
#> 272                            Crushed    400 <NA> google
#> 273                             Cuprum    400 <NA> google
#> 274                          Cute Font    400 <NA> google
#> 275                             Cutive    400 <NA> google
#> 276                        Cutive Mono    400 <NA> google
#> 277                            DM Mono    400 <NA> google
#> 278                            DM Sans    400 <NA> google
#> 279                   DM Serif Display    400 <NA> google
#> 280                      DM Serif Text    400 <NA> google
#> 281                             Damion    400 <NA> google
#> 282                     Dancing Script    400 <NA> google
#> 283                            Dangrek    400 <NA> google
#> 284                   Darker Grotesque    400 <NA> google
#> 285                        David Libre    400 <NA> google
#> 286               Dawning of a New Day    400 <NA> google
#> 287                           Days One    400 <NA> google
#> 288                              Dekko    400 <NA> google
#> 289                    Dela Gothic One    400 <NA> google
#> 290                             Delius    400 <NA> google
#> 291                  Delius Swash Caps    400 <NA> google
#> 292                     Delius Unicase    400 <NA> google
#> 293                      Della Respira    400 <NA> google
#> 294                           Denk One    400 <NA> google
#> 295                         Devonshire    400 <NA> google
#> 296                           Dhurjati    400 <NA> google
#> 297                      Didact Gothic    400 <NA> google
#> 298                          Diplomata    400 <NA> google
#> 299                       Diplomata SC    400 <NA> google
#> 300                           Do Hyeon    400 <NA> google
#> 301                              Dokdo    400 <NA> google
#> 302                             Domine    400 <NA> google
#> 303                        Donegal One    400 <NA> google
#> 304                             Dongle    400 <NA> google
#> 305                         Doppio One    400 <NA> google
#> 306                              Dorsa    400 <NA> google
#> 307                              Dosis    400 <NA> google
#> 308                        DotGothic16    400 <NA> google
#> 309                        Dr Sugiyama    400 <NA> google
#> 310                          Duru Sans    400 <NA> google
#> 311                          Dynalight    400 <NA> google
#> 312                        EB Garamond    400 <NA> google
#> 313                         Eagle Lake    400 <NA> google
#> 314                     East Sea Dokdo    400 <NA> google
#> 315                              Eater    400 <NA> google
#> 316                          Economica    400 <NA> google
#> 317                              Eczar    400 <NA> google
#> 318                         El Messiri    400 <NA> google
#> 319                        Electrolize    400 <NA> google
#> 320                              Elsie    400 <NA> google
#> 321                   Elsie Swash Caps    400 <NA> google
#> 322                        Emblema One    400 <NA> google
#> 323                       Emilys Candy    400 <NA> google
#> 324                        Encode Sans    400 <NA> google
#> 325              Encode Sans Condensed    400 <NA> google
#> 326               Encode Sans Expanded    400 <NA> google
#> 327                     Encode Sans SC    400 <NA> google
#> 328         Encode Sans Semi Condensed    400 <NA> google
#> 329          Encode Sans Semi Expanded    400 <NA> google
#> 330                         Engagement    400 <NA> google
#> 331                          Englebert    400 <NA> google
#> 332                          Enriqueta    400 <NA> google
#> 333                            Ephesis    400 <NA> google
#> 334                           Epilogue    400 <NA> google
#> 335                          Erica One    400 <NA> google
#> 336                            Esteban    400 <NA> google
#> 337                            Estonia    400 <NA> google
#> 338                    Euphoria Script    400 <NA> google
#> 339                              Ewert    400 <NA> google
#> 340                                Exo    400 <NA> google
#> 341                              Exo 2    400 <NA> google
#> 342                      Expletus Sans    400 <NA> google
#> 343                            Explora    400 <NA> google
#> 344                           Fahkwang    400 <NA> google
#> 345                       Fanwood Text    400 <NA> google
#> 346                              Farro    400 <NA> google
#> 347                             Farsan    400 <NA> google
#> 348                          Fascinate    400 <NA> google
#> 349                   Fascinate Inline    400 <NA> google
#> 350                         Faster One    400 <NA> google
#> 351                           Fasthand    400 <NA> google
#> 352                          Fauna One    400 <NA> google
#> 353                           Faustina    400 <NA> google
#> 354                           Federant    400 <NA> google
#> 355                             Federo    400 <NA> google
#> 356                             Felipa    400 <NA> google
#> 357                              Fenix    400 <NA> google
#> 358                            Festive    400 <NA> google
#> 359                       Finger Paint    400 <NA> google
#> 360                          Fira Code    400 <NA> google
#> 361                          Fira Mono    400 <NA> google
#> 362                          Fira Sans    400 <NA> google
#> 363                Fira Sans Condensed    400 <NA> google
#> 364          Fira Sans Extra Condensed    400 <NA> google
#> 365                         Fjalla One    400 <NA> google
#> 366                          Fjord One    400 <NA> google
#> 367                           Flamenco    400 <NA> google
#> 368                            Flavors    400 <NA> google
#> 369                      Fleur De Leah    400 <NA> google
#> 370                         Flow Block    400 <NA> google
#> 371                      Flow Circular    400 <NA> google
#> 372                       Flow Rounded    400 <NA> google
#> 373                         Fondamento    400 <NA> google
#> 374                   Fontdiner Swanky    400 <NA> google
#> 375                              Forum    400 <NA> google
#> 376                       Francois One    400 <NA> google
#> 377                   Frank Ruhl Libre    400 <NA> google
#> 378                           Fraunces    400 <NA> google
#> 379                       Freckle Face    400 <NA> google
#> 380               Fredericka the Great    400 <NA> google
#> 381                            Fredoka    400 <NA> google
#> 382                        Fredoka One    400 <NA> google
#> 383                           Freehand    400 <NA> google
#> 384                             Fresca    400 <NA> google
#> 385                            Frijole    400 <NA> google
#> 386                            Fruktur    400 <NA> google
#> 387                          Fugaz One    400 <NA> google
#> 388                            Fuggles    400 <NA> google
#> 389                      Fuzzy Bubbles    400 <NA> google
#> 390                          GFS Didot    400 <NA> google
#> 391                    GFS Neohellenic    400 <NA> google
#> 392                           Gabriela    400 <NA> google
#> 393                              Gaegu    400 <NA> google
#> 394                             Gafata    400 <NA> google
#> 395                             Galada    400 <NA> google
#> 396                           Galdeano    400 <NA> google
#> 397                            Galindo    400 <NA> google
#> 398                       Gamja Flower    400 <NA> google
#> 399                           Gayathri    400 <NA> google
#> 400                            Gelasio    400 <NA> google
#> 401                       Gemunu Libre    400 <NA> google
#> 402                              Genos    400 <NA> google
#> 403                      Gentium Basic    400 <NA> google
#> 404                 Gentium Book Basic    400 <NA> google
#> 405                                Geo    400 <NA> google
#> 406                            Georama    400 <NA> google
#> 407                            Geostar    400 <NA> google
#> 408                       Geostar Fill    400 <NA> google
#> 409                       Germania One    400 <NA> google
#> 410                       Gideon Roman    400 <NA> google
#> 411                             Gidugu    400 <NA> google
#> 412                      Gilda Display    400 <NA> google
#> 413                           Girassol    400 <NA> google
#> 414                     Give You Glory    400 <NA> google
#> 415                      Glass Antiqua    400 <NA> google
#> 416                             Glegoo    400 <NA> google
#> 417                  Gloria Hallelujah    400 <NA> google
#> 418                              Glory    400 <NA> google
#> 419                             Gluten    400 <NA> google
#> 420                         Goblin One    400 <NA> google
#> 421                         Gochi Hand    400 <NA> google
#> 422                            Goldman    400 <NA> google
#> 423                           Gorditas    400 <NA> google
#> 424                          Gothic A1    400 <NA> google
#> 425                               Gotu    400 <NA> google
#> 426              Goudy Bookletter 1911    400 <NA> google
#> 427                       Gowun Batang    400 <NA> google
#> 428                        Gowun Dodum    400 <NA> google
#> 429                           Graduate    400 <NA> google
#> 430                        Grand Hotel    400 <NA> google
#> 431                       Grandstander    400 <NA> google
#> 432                       Gravitas One    400 <NA> google
#> 433                        Great Vibes    400 <NA> google
#> 434                     Grechen Fuemen    400 <NA> google
#> 435                             Grenze    400 <NA> google
#> 436                     Grenze Gotisch    400 <NA> google
#> 437                            Grey Qo    400 <NA> google
#> 438                             Griffy    400 <NA> google
#> 439                             Gruppo    400 <NA> google
#> 440                              Gudea    400 <NA> google
#> 441                               Gugi    400 <NA> google
#> 442                             Gupter    400 <NA> google
#> 443                           Gurajada    400 <NA> google
#> 444                          Gwendolyn    400 <NA> google
#> 445                             Habibi    400 <NA> google
#> 446                     Hachi Maru Pop    400 <NA> google
#> 447                            Hahmlet    400 <NA> google
#> 448                             Halant    400 <NA> google
#> 449                    Hammersmith One    400 <NA> google
#> 450                            Hanalei    400 <NA> google
#> 451                       Hanalei Fill    400 <NA> google
#> 452                            Handlee    400 <NA> google
#> 453                            Hanuman    400 <NA> google
#> 454                       Happy Monkey    400 <NA> google
#> 455                          Harmattan    400 <NA> google
#> 456                       Headland One    400 <NA> google
#> 457                              Heebo    400 <NA> google
#> 458                        Henny Penny    400 <NA> google
#> 459                         Hepta Slab    400 <NA> google
#> 460               Herr Von Muellerhoff    400 <NA> google
#> 461                          Hi Melody    400 <NA> google
#> 462                        Hina Mincho    400 <NA> google
#> 463                               Hind    400 <NA> google
#> 464                        Hind Guntur    400 <NA> google
#> 465                       Hind Madurai    400 <NA> google
#> 466                      Hind Siliguri    400 <NA> google
#> 467                      Hind Vadodara    400 <NA> google
#> 468                    Holtwood One SC    400 <NA> google
#> 469                     Homemade Apple    400 <NA> google
#> 470                           Homenaje    400 <NA> google
#> 471                           Hubballi    400 <NA> google
#> 472                          Hurricane    400 <NA> google
#> 473                      IBM Plex Mono    400 <NA> google
#> 474                      IBM Plex Sans    400 <NA> google
#> 475               IBM Plex Sans Arabic    400 <NA> google
#> 476            IBM Plex Sans Condensed    400 <NA> google
#> 477           IBM Plex Sans Devanagari    400 <NA> google
#> 478               IBM Plex Sans Hebrew    400 <NA> google
#> 479                   IBM Plex Sans KR    400 <NA> google
#> 480                 IBM Plex Sans Thai    400 <NA> google
#> 481          IBM Plex Sans Thai Looped    400 <NA> google
#> 482                     IBM Plex Serif    400 <NA> google
#> 483                    IM Fell DW Pica    400 <NA> google
#> 484                 IM Fell DW Pica SC    400 <NA> google
#> 485                IM Fell Double Pica    400 <NA> google
#> 486             IM Fell Double Pica SC    400 <NA> google
#> 487                    IM Fell English    400 <NA> google
#> 488                 IM Fell English SC    400 <NA> google
#> 489               IM Fell French Canon    400 <NA> google
#> 490            IM Fell French Canon SC    400 <NA> google
#> 491               IM Fell Great Primer    400 <NA> google
#> 492            IM Fell Great Primer SC    400 <NA> google
#> 493                   Ibarra Real Nova    400 <NA> google
#> 494                            Iceberg    400 <NA> google
#> 495                            Iceland    400 <NA> google
#> 496                              Imbue    400 <NA> google
#> 497                    Imperial Script    400 <NA> google
#> 498                            Imprima    400 <NA> google
#> 499                        Inconsolata    400 <NA> google
#> 500                              Inder    400 <NA> google
#> 501                       Indie Flower    400 <NA> google
#> 502                              Inika    400 <NA> google
#> 503                     Inknut Antiqua    400 <NA> google
#> 504                         Inria Sans    400 <NA> google
#> 505                        Inria Serif    400 <NA> google
#> 506                        Inspiration    400 <NA> google
#> 507                              Inter    400 <NA> google
#> 508                       Irish Grover    400 <NA> google
#> 509                     Island Moments    400 <NA> google
#> 510                          Istok Web    400 <NA> google
#> 511                           Italiana    400 <NA> google
#> 512                          Italianno    400 <NA> google
#> 513                               Itim    400 <NA> google
#> 514                   Jacques Francois    400 <NA> google
#> 515            Jacques Francois Shadow    400 <NA> google
#> 516                              Jaldi    400 <NA> google
#> 517                     JetBrains Mono    400 <NA> google
#> 518                     Jim Nightshade    400 <NA> google
#> 519                         Jockey One    400 <NA> google
#> 520                       Jolly Lodger    400 <NA> google
#> 521                           Jomhuria    400 <NA> google
#> 522                          Jomolhari    400 <NA> google
#> 523                       Josefin Sans    400 <NA> google
#> 524                       Josefin Slab    400 <NA> google
#> 525                               Jost    400 <NA> google
#> 526                           Joti One    400 <NA> google
#> 527                                Jua    400 <NA> google
#> 528                             Judson    400 <NA> google
#> 529                              Julee    400 <NA> google
#> 530                    Julius Sans One    400 <NA> google
#> 531                              Junge    400 <NA> google
#> 532                               Jura    400 <NA> google
#> 533                  Just Another Hand    400 <NA> google
#> 534            Just Me Again Down Here    400 <NA> google
#> 535                                K2D    400 <NA> google
#> 536                              Kadwa    400 <NA> google
#> 537                       Kaisei Decol    400 <NA> google
#> 538                   Kaisei HarunoUmi    400 <NA> google
#> 539                        Kaisei Opti    400 <NA> google
#> 540                     Kaisei Tokumin    400 <NA> google
#> 541                              Kalam    400 <NA> google
#> 542                            Kameron    400 <NA> google
#> 543                              Kanit    400 <NA> google
#> 544                          Kantumruy    400 <NA> google
#> 545                          Karantina    400 <NA> google
#> 546                              Karla    400 <NA> google
#> 547                              Karma    400 <NA> google
#> 548                            Katibeh    400 <NA> google
#> 549                     Kaushan Script    400 <NA> google
#> 550                          Kavivanar    400 <NA> google
#> 551                             Kavoon    400 <NA> google
#> 552                         Kdam Thmor    400 <NA> google
#> 553                         Keania One    400 <NA> google
#> 554                         Kelly Slab    400 <NA> google
#> 555                              Kenia    400 <NA> google
#> 556                              Khand    400 <NA> google
#> 557                              Khmer    400 <NA> google
#> 558                              Khula    400 <NA> google
#> 559                              Kings    400 <NA> google
#> 560                     Kirang Haerang    400 <NA> google
#> 561                           Kite One    400 <NA> google
#> 562                          Kiwi Maru    400 <NA> google
#> 563                           Klee One    400 <NA> google
#> 564                            Knewave    400 <NA> google
#> 565                               KoHo    400 <NA> google
#> 566                          Kodchasan    400 <NA> google
#> 567                     Koh Santepheap    400 <NA> google
#> 568                       Kolker Brush    400 <NA> google
#> 569                             Kosugi    400 <NA> google
#> 570                        Kosugi Maru    400 <NA> google
#> 571                          Kotta One    400 <NA> google
#> 572                             Koulen    400 <NA> google
#> 573                             Kranky    400 <NA> google
#> 574                              Kreon    400 <NA> google
#> 575                             Kristi    400 <NA> google
#> 576                          Krona One    400 <NA> google
#> 577                               Krub    400 <NA> google
#> 578                              Kufam    400 <NA> google
#> 579                         Kulim Park    400 <NA> google
#> 580                          Kumar One    400 <NA> google
#> 581                  Kumar One Outline    400 <NA> google
#> 582                         Kumbh Sans    400 <NA> google
#> 583                             Kurale    400 <NA> google
#> 584                    La Belle Aurore    400 <NA> google
#> 585                            Lacquer    400 <NA> google
#> 586                              Laila    400 <NA> google
#> 587                        Lakki Reddy    400 <NA> google
#> 588                            Lalezar    400 <NA> google
#> 589                           Lancelot    400 <NA> google
#> 590                             Langar    400 <NA> google
#> 591                             Lateef    400 <NA> google
#> 592                               Lato    400 <NA> google
#> 593                      League Gothic    400 <NA> google
#> 594                      League Script    400 <NA> google
#> 595                     League Spartan    400 <NA> google
#> 596                       Leckerli One    400 <NA> google
#> 597                             Ledger    400 <NA> google
#> 598                             Lekton    400 <NA> google
#> 599                              Lemon    400 <NA> google
#> 600                           Lemonada    400 <NA> google
#> 601                             Lexend    400 <NA> google
#> 602                        Lexend Deca    400 <NA> google
#> 603                         Lexend Exa    400 <NA> google
#> 604                        Lexend Giga    400 <NA> google
#> 605                        Lexend Mega    400 <NA> google
#> 606                        Lexend Peta    400 <NA> google
#> 607                        Lexend Tera    400 <NA> google
#> 608                       Lexend Zetta    400 <NA> google
#> 609                  Libre Barcode 128    400 <NA> google
#> 610             Libre Barcode 128 Text    400 <NA> google
#> 611                   Libre Barcode 39    400 <NA> google
#> 612          Libre Barcode 39 Extended    400 <NA> google
#> 613     Libre Barcode 39 Extended Text    400 <NA> google
#> 614              Libre Barcode 39 Text    400 <NA> google
#> 615           Libre Barcode EAN13 Text    400 <NA> google
#> 616                  Libre Baskerville    400 <NA> google
#> 617               Libre Caslon Display    400 <NA> google
#> 618                  Libre Caslon Text    400 <NA> google
#> 619                     Libre Franklin    400 <NA> google
#> 620                           Licorice    400 <NA> google
#> 621                        Life Savers    400 <NA> google
#> 622                         Lilita One    400 <NA> google
#> 623                    Lily Script One    400 <NA> google
#> 624                          Limelight    400 <NA> google
#> 625                        Linden Hill    400 <NA> google
#> 626                           Literata    400 <NA> google
#> 627                   Liu Jian Mao Cao    400 <NA> google
#> 628                             Livvic    400 <NA> google
#> 629                            Lobster    400 <NA> google
#> 630                        Lobster Two    400 <NA> google
#> 631                   Londrina Outline    400 <NA> google
#> 632                    Londrina Shadow    400 <NA> google
#> 633                    Londrina Sketch    400 <NA> google
#> 634                     Londrina Solid    400 <NA> google
#> 635                          Long Cang    400 <NA> google
#> 636                               Lora    400 <NA> google
#> 637                         Love Light    400 <NA> google
#> 638              Love Ya Like A Sister    400 <NA> google
#> 639                  Loved by the King    400 <NA> google
#> 640                     Lovers Quarrel    400 <NA> google
#> 641                       Luckiest Guy    400 <NA> google
#> 642                           Lusitana    400 <NA> google
#> 643                            Lustria    400 <NA> google
#> 644                    Luxurious Roman    400 <NA> google
#> 645                   Luxurious Script    400 <NA> google
#> 646                           M PLUS 1    400 <NA> google
#> 647                      M PLUS 1 Code    400 <NA> google
#> 648                          M PLUS 1p    400 <NA> google
#> 649                           M PLUS 2    400 <NA> google
#> 650                  M PLUS Code Latin    400 <NA> google
#> 651                  M PLUS Rounded 1c    400 <NA> google
#> 652                      Ma Shan Zheng    400 <NA> google
#> 653                            Macondo    400 <NA> google
#> 654                 Macondo Swash Caps    400 <NA> google
#> 655                               Mada    400 <NA> google
#> 656                              Magra    400 <NA> google
#> 657                      Maiden Orange    400 <NA> google
#> 658                            Maitree    400 <NA> google
#> 659                 Major Mono Display    400 <NA> google
#> 660                               Mako    400 <NA> google
#> 661                               Mali    400 <NA> google
#> 662                           Mallanna    400 <NA> google
#> 663                            Mandali    400 <NA> google
#> 664                            Manjari    400 <NA> google
#> 665                            Manrope    400 <NA> google
#> 666                           Mansalva    400 <NA> google
#> 667                            Manuale    400 <NA> google
#> 668                          Marcellus    400 <NA> google
#> 669                       Marcellus SC    400 <NA> google
#> 670                       Marck Script    400 <NA> google
#> 671                          Margarine    400 <NA> google
#> 672                       Markazi Text    400 <NA> google
#> 673                          Marko One    400 <NA> google
#> 674                           Marmelad    400 <NA> google
#> 675                             Martel    400 <NA> google
#> 676                        Martel Sans    400 <NA> google
#> 677                             Marvel    400 <NA> google
#> 678                               Mate    400 <NA> google
#> 679                            Mate SC    400 <NA> google
#> 680                          Maven Pro    400 <NA> google
#> 681                            McLaren    400 <NA> google
#> 682                          Mea Culpa    400 <NA> google
#> 683                             Meddon    400 <NA> google
#> 684                      MedievalSharp    400 <NA> google
#> 685                         Medula One    400 <NA> google
#> 686                       Meera Inimai    400 <NA> google
#> 687                             Megrim    400 <NA> google
#> 688                        Meie Script    400 <NA> google
#> 689                        Meow Script    400 <NA> google
#> 690                           Merienda    400 <NA> google
#> 691                       Merienda One    400 <NA> google
#> 692                       Merriweather    400 <NA> google
#> 693                  Merriweather Sans    400 <NA> google
#> 694                              Metal    400 <NA> google
#> 695                        Metal Mania    400 <NA> google
#> 696                       Metamorphous    400 <NA> google
#> 697                        Metrophobic    400 <NA> google
#> 698                           Michroma    400 <NA> google
#> 699                            Milonga    400 <NA> google
#> 700                          Miltonian    400 <NA> google
#> 701                   Miltonian Tattoo    400 <NA> google
#> 702                               Mina    400 <NA> google
#> 703                            Miniver    400 <NA> google
#> 704                       Miriam Libre    400 <NA> google
#> 705                              Mirza    400 <NA> google
#> 706                     Miss Fajardose    400 <NA> google
#> 707                               Mitr    400 <NA> google
#> 708                     Mochiy Pop One    400 <NA> google
#> 709                   Mochiy Pop P One    400 <NA> google
#> 710                              Modak    400 <NA> google
#> 711                     Modern Antiqua    400 <NA> google
#> 712                              Mogra    400 <NA> google
#> 713                             Mohave    400 <NA> google
#> 714                            Molengo    400 <NA> google
#> 715                              Molle    400 <NA> google
#> 716                              Monda    400 <NA> google
#> 717                           Monofett    400 <NA> google
#> 718                            Monoton    400 <NA> google
#> 719               Monsieur La Doulaise    400 <NA> google
#> 720                            Montaga    400 <NA> google
#> 721                       Montagu Slab    400 <NA> google
#> 722                         MonteCarlo    400 <NA> google
#> 723                             Montez    400 <NA> google
#> 724                         Montserrat    400 <NA> google
#> 725              Montserrat Alternates    400 <NA> google
#> 726               Montserrat Subrayada    400 <NA> google
#> 727                        Moo Lah Lah    400 <NA> google
#> 728                         Moon Dance    400 <NA> google
#> 729                               Moul    400 <NA> google
#> 730                           Moulpali    400 <NA> google
#> 731             Mountains of Christmas    400 <NA> google
#> 732                      Mouse Memoirs    400 <NA> google
#> 733                         Mr Bedfort    400 <NA> google
#> 734                           Mr Dafoe    400 <NA> google
#> 735                     Mr De Haviland    400 <NA> google
#> 736                Mrs Saint Delafield    400 <NA> google
#> 737                      Mrs Sheppards    400 <NA> google
#> 738                              Mukta    400 <NA> google
#> 739                        Mukta Mahee    400 <NA> google
#> 740                        Mukta Malar    400 <NA> google
#> 741                        Mukta Vaani    400 <NA> google
#> 742                             Mulish    400 <NA> google
#> 743                            Murecho    400 <NA> google
#> 744                       MuseoModerno    400 <NA> google
#> 745                      Mystery Quest    400 <NA> google
#> 746                                NTR    400 <NA> google
#> 747                 Nanum Brush Script    400 <NA> google
#> 748                       Nanum Gothic    400 <NA> google
#> 749                Nanum Gothic Coding    400 <NA> google
#> 750                     Nanum Myeongjo    400 <NA> google
#> 751                   Nanum Pen Script    400 <NA> google
#> 752                        Neonderthaw    400 <NA> google
#> 753                          Nerko One    400 <NA> google
#> 754                             Neucha    400 <NA> google
#> 755                             Neuton    400 <NA> google
#> 756                         New Rocker    400 <NA> google
#> 757                        New Tegomin    400 <NA> google
#> 758                         News Cycle    400 <NA> google
#> 759                         Newsreader    400 <NA> google
#> 760                            Niconne    400 <NA> google
#> 761                            Niramit    400 <NA> google
#> 762                          Nixie One    400 <NA> google
#> 763                             Nobile    400 <NA> google
#> 764                             Nokora    400 <NA> google
#> 765                            Norican    400 <NA> google
#> 766                            Nosifer    400 <NA> google
#> 767                            Notable    400 <NA> google
#> 768               Nothing You Could Do    400 <NA> google
#> 769                       Noticia Text    400 <NA> google
#> 770                   Noto Kufi Arabic    400 <NA> google
#> 771                         Noto Music    400 <NA> google
#> 772                  Noto Naskh Arabic    400 <NA> google
#> 773                 Noto Nastaliq Urdu    400 <NA> google
#> 774                  Noto Rashi Hebrew    400 <NA> google
#> 775                          Noto Sans    400 <NA> google
#> 776                    Noto Sans Adlam    400 <NA> google
#> 777           Noto Sans Adlam Unjoined    400 <NA> google
#> 778    Noto Sans Anatolian Hieroglyphs    400 <NA> google
#> 779                   Noto Sans Arabic    400 <NA> google
#> 780                 Noto Sans Armenian    400 <NA> google
#> 781                  Noto Sans Avestan    400 <NA> google
#> 782                 Noto Sans Balinese    400 <NA> google
#> 783                    Noto Sans Bamum    400 <NA> google
#> 784                Noto Sans Bassa Vah    400 <NA> google
#> 785                    Noto Sans Batak    400 <NA> google
#> 786                  Noto Sans Bengali    400 <NA> google
#> 787                Noto Sans Bhaiksuki    400 <NA> google
#> 788                   Noto Sans Brahmi    400 <NA> google
#> 789                 Noto Sans Buginese    400 <NA> google
#> 790                    Noto Sans Buhid    400 <NA> google
#> 791      Noto Sans Canadian Aboriginal    400 <NA> google
#> 792                   Noto Sans Carian    400 <NA> google
#> 793       Noto Sans Caucasian Albanian    400 <NA> google
#> 794                   Noto Sans Chakma    400 <NA> google
#> 795                     Noto Sans Cham    400 <NA> google
#> 796                 Noto Sans Cherokee    400 <NA> google
#> 797                   Noto Sans Coptic    400 <NA> google
#> 798                Noto Sans Cuneiform    400 <NA> google
#> 799                  Noto Sans Cypriot    400 <NA> google
#> 800                  Noto Sans Deseret    400 <NA> google
#> 801               Noto Sans Devanagari    400 <NA> google
#> 802                  Noto Sans Display    400 <NA> google
#> 803                 Noto Sans Duployan    400 <NA> google
#> 804     Noto Sans Egyptian Hieroglyphs    400 <NA> google
#> 805                  Noto Sans Elbasan    400 <NA> google
#> 806                  Noto Sans Elymaic    400 <NA> google
#> 807                 Noto Sans Georgian    400 <NA> google
#> 808               Noto Sans Glagolitic    400 <NA> google
#> 809                   Noto Sans Gothic    400 <NA> google
#> 810                  Noto Sans Grantha    400 <NA> google
#> 811                 Noto Sans Gujarati    400 <NA> google
#> 812            Noto Sans Gunjala Gondi    400 <NA> google
#> 813                 Noto Sans Gurmukhi    400 <NA> google
#> 814                       Noto Sans HK    400 <NA> google
#> 815          Noto Sans Hanifi Rohingya    400 <NA> google
#> 816                  Noto Sans Hanunoo    400 <NA> google
#> 817                   Noto Sans Hatran    400 <NA> google
#> 818                   Noto Sans Hebrew    400 <NA> google
#> 819         Noto Sans Imperial Aramaic    400 <NA> google
#> 820      Noto Sans Indic Siyaq Numbers    400 <NA> google
#> 821    Noto Sans Inscriptional Pahlavi    400 <NA> google
#> 822   Noto Sans Inscriptional Parthian    400 <NA> google
#> 823                       Noto Sans JP    400 <NA> google
#> 824                 Noto Sans Javanese    400 <NA> google
#> 825                       Noto Sans KR    400 <NA> google
#> 826                   Noto Sans Kaithi    400 <NA> google
#> 827                  Noto Sans Kannada    400 <NA> google
#> 828                 Noto Sans Kayah Li    400 <NA> google
#> 829               Noto Sans Kharoshthi    400 <NA> google
#> 830                    Noto Sans Khmer    400 <NA> google
#> 831                   Noto Sans Khojki    400 <NA> google
#> 832                Noto Sans Khudawadi    400 <NA> google
#> 833                      Noto Sans Lao    400 <NA> google
#> 834                   Noto Sans Lepcha    400 <NA> google
#> 835                    Noto Sans Limbu    400 <NA> google
#> 836                 Noto Sans Linear A    400 <NA> google
#> 837                 Noto Sans Linear B    400 <NA> google
#> 838                     Noto Sans Lisu    400 <NA> google
#> 839                   Noto Sans Lycian    400 <NA> google
#> 840                   Noto Sans Lydian    400 <NA> google
#> 841                 Noto Sans Mahajani    400 <NA> google
#> 842                Noto Sans Malayalam    400 <NA> google
#> 843                  Noto Sans Mandaic    400 <NA> google
#> 844               Noto Sans Manichaean    400 <NA> google
#> 845                  Noto Sans Marchen    400 <NA> google
#> 846            Noto Sans Masaram Gondi    400 <NA> google
#> 847                     Noto Sans Math    400 <NA> google
#> 848           Noto Sans Mayan Numerals    400 <NA> google
#> 849              Noto Sans Medefaidrin    400 <NA> google
#> 850             Noto Sans Meetei Mayek    400 <NA> google
#> 851                 Noto Sans Meroitic    400 <NA> google
#> 852                     Noto Sans Miao    400 <NA> google
#> 853                     Noto Sans Modi    400 <NA> google
#> 854                Noto Sans Mongolian    400 <NA> google
#> 855                     Noto Sans Mono    400 <NA> google
#> 856                      Noto Sans Mro    400 <NA> google
#> 857                  Noto Sans Multani    400 <NA> google
#> 858                  Noto Sans Myanmar    400 <NA> google
#> 859                     Noto Sans N Ko    400 <NA> google
#> 860                Noto Sans Nabataean    400 <NA> google
#> 861              Noto Sans New Tai Lue    400 <NA> google
#> 862                     Noto Sans Newa    400 <NA> google
#> 863                    Noto Sans Nushu    400 <NA> google
#> 864                    Noto Sans Ogham    400 <NA> google
#> 865                 Noto Sans Ol Chiki    400 <NA> google
#> 866            Noto Sans Old Hungarian    400 <NA> google
#> 867               Noto Sans Old Italic    400 <NA> google
#> 868        Noto Sans Old North Arabian    400 <NA> google
#> 869               Noto Sans Old Permic    400 <NA> google
#> 870              Noto Sans Old Persian    400 <NA> google
#> 871              Noto Sans Old Sogdian    400 <NA> google
#> 872        Noto Sans Old South Arabian    400 <NA> google
#> 873               Noto Sans Old Turkic    400 <NA> google
#> 874                    Noto Sans Oriya    400 <NA> google
#> 875                    Noto Sans Osage    400 <NA> google
#> 876                  Noto Sans Osmanya    400 <NA> google
#> 877             Noto Sans Pahawh Hmong    400 <NA> google
#> 878                Noto Sans Palmyrene    400 <NA> google
#> 879              Noto Sans Pau Cin Hau    400 <NA> google
#> 880                 Noto Sans Phags Pa    400 <NA> google
#> 881               Noto Sans Phoenician    400 <NA> google
#> 882          Noto Sans Psalter Pahlavi    400 <NA> google
#> 883                   Noto Sans Rejang    400 <NA> google
#> 884                    Noto Sans Runic    400 <NA> google
#> 885                       Noto Sans SC    400 <NA> google
#> 886                Noto Sans Samaritan    400 <NA> google
#> 887               Noto Sans Saurashtra    400 <NA> google
#> 888                  Noto Sans Sharada    400 <NA> google
#> 889                  Noto Sans Shavian    400 <NA> google
#> 890                  Noto Sans Siddham    400 <NA> google
#> 891                  Noto Sans Sinhala    400 <NA> google
#> 892                  Noto Sans Sogdian    400 <NA> google
#> 893             Noto Sans Sora Sompeng    400 <NA> google
#> 894                  Noto Sans Soyombo    400 <NA> google
#> 895                Noto Sans Sundanese    400 <NA> google
#> 896             Noto Sans Syloti Nagri    400 <NA> google
#> 897                  Noto Sans Symbols    400 <NA> google
#> 898                Noto Sans Symbols 2    400 <NA> google
#> 899                   Noto Sans Syriac    400 <NA> google
#> 900                       Noto Sans TC    400 <NA> google
#> 901                  Noto Sans Tagalog    400 <NA> google
#> 902                 Noto Sans Tagbanwa    400 <NA> google
#> 903                   Noto Sans Tai Le    400 <NA> google
#> 904                 Noto Sans Tai Tham    400 <NA> google
#> 905                 Noto Sans Tai Viet    400 <NA> google
#> 906                    Noto Sans Takri    400 <NA> google
#> 907                    Noto Sans Tamil    400 <NA> google
#> 908         Noto Sans Tamil Supplement    400 <NA> google
#> 909                   Noto Sans Telugu    400 <NA> google
#> 910                   Noto Sans Thaana    400 <NA> google
#> 911                     Noto Sans Thai    400 <NA> google
#> 912              Noto Sans Thai Looped    400 <NA> google
#> 913                 Noto Sans Tifinagh    400 <NA> google
#> 914                  Noto Sans Tirhuta    400 <NA> google
#> 915                 Noto Sans Ugaritic    400 <NA> google
#> 916                      Noto Sans Vai    400 <NA> google
#> 917                   Noto Sans Wancho    400 <NA> google
#> 918              Noto Sans Warang Citi    400 <NA> google
#> 919                       Noto Sans Yi    400 <NA> google
#> 920         Noto Sans Zanabazar Square    400 <NA> google
#> 921                         Noto Serif    400 <NA> google
#> 922                    Noto Serif Ahom    400 <NA> google
#> 923                Noto Serif Armenian    400 <NA> google
#> 924                Noto Serif Balinese    400 <NA> google
#> 925                 Noto Serif Bengali    400 <NA> google
#> 926              Noto Serif Devanagari    400 <NA> google
#> 927                 Noto Serif Display    400 <NA> google
#> 928                   Noto Serif Dogra    400 <NA> google
#> 929                Noto Serif Ethiopic    400 <NA> google
#> 930                Noto Serif Georgian    400 <NA> google
#> 931                 Noto Serif Grantha    400 <NA> google
#> 932                Noto Serif Gujarati    400 <NA> google
#> 933                Noto Serif Gurmukhi    400 <NA> google
#> 934                  Noto Serif Hebrew    400 <NA> google
#> 935                      Noto Serif JP    400 <NA> google
#> 936                      Noto Serif KR    400 <NA> google
#> 937                 Noto Serif Kannada    400 <NA> google
#> 938                   Noto Serif Khmer    400 <NA> google
#> 939                     Noto Serif Lao    400 <NA> google
#> 940               Noto Serif Malayalam    400 <NA> google
#> 941                 Noto Serif Myanmar    400 <NA> google
#> 942  Noto Serif Nyiakeng Puachue Hmong    400 <NA> google
#> 943                      Noto Serif SC    400 <NA> google
#> 944                 Noto Serif Sinhala    400 <NA> google
#> 945                      Noto Serif TC    400 <NA> google
#> 946                   Noto Serif Tamil    400 <NA> google
#> 947                  Noto Serif Tangut    400 <NA> google
#> 948                  Noto Serif Telugu    400 <NA> google
#> 949                    Noto Serif Thai    400 <NA> google
#> 950                 Noto Serif Tibetan    400 <NA> google
#> 951                  Noto Serif Yezidi    400 <NA> google
#> 952             Noto Traditional Nushu    400 <NA> google
#> 953                           Nova Cut    400 <NA> google
#> 954                          Nova Flat    400 <NA> google
#> 955                          Nova Mono    400 <NA> google
#> 956                          Nova Oval    400 <NA> google
#> 957                         Nova Round    400 <NA> google
#> 958                        Nova Script    400 <NA> google
#> 959                          Nova Slim    400 <NA> google
#> 960                        Nova Square    400 <NA> google
#> 961                             Numans    400 <NA> google
#> 962                             Nunito    400 <NA> google
#> 963                        Nunito Sans    400 <NA> google
#> 964                        Odibee Sans    400 <NA> google
#> 965                     Odor Mean Chey    400 <NA> google
#> 966                            Offside    400 <NA> google
#> 967                                 Oi    400 <NA> google
#> 968                    Old Standard TT    400 <NA> google
#> 969                          Oldenburg    400 <NA> google
#> 970                                Ole    400 <NA> google
#> 971                        Oleo Script    400 <NA> google
#> 972             Oleo Script Swash Caps    400 <NA> google
#> 973                          Oooh Baby    400 <NA> google
#> 974                          Open Sans    400 <NA> google
#> 975                        Oranienbaum    400 <NA> google
#> 976                           Orbitron    400 <NA> google
#> 977                            Oregano    400 <NA> google
#> 978                        Orelega One    400 <NA> google
#> 979                            Orienta    400 <NA> google
#> 980                    Original Surfer    400 <NA> google
#> 981                             Oswald    400 <NA> google
#> 982                     Otomanopee One    400 <NA> google
#> 983                             Outfit    400 <NA> google
#> 984                   Over the Rainbow    400 <NA> google
#> 985                           Overlock    400 <NA> google
#> 986                        Overlock SC    400 <NA> google
#> 987                           Overpass    400 <NA> google
#> 988                      Overpass Mono    400 <NA> google
#> 989                                Ovo    400 <NA> google
#> 990                            Oxanium    400 <NA> google
#> 991                             Oxygen    400 <NA> google
#> 992                        Oxygen Mono    400 <NA> google
#> 993                            PT Mono    400 <NA> google
#> 994                            PT Sans    400 <NA> google
#> 995                    PT Sans Caption    400 <NA> google
#> 996                     PT Sans Narrow    400 <NA> google
#> 997                           PT Serif    400 <NA> google
#> 998                   PT Serif Caption    400 <NA> google
#> 999                           Pacifico    400 <NA> google
#> 1000                            Padauk    400 <NA> google
#> 1001                         Palanquin    400 <NA> google
#> 1002                    Palanquin Dark    400 <NA> google
#> 1003                    Palette Mosaic    400 <NA> google
#> 1004                          Pangolin    400 <NA> google
#> 1005                           Paprika    400 <NA> google
#> 1006                        Parisienne    400 <NA> google
#> 1007                       Passero One    400 <NA> google
#> 1008                       Passion One    400 <NA> google
#> 1009                 Passions Conflict    400 <NA> google
#> 1010                Pathway Gothic One    400 <NA> google
#> 1011                      Patrick Hand    400 <NA> google
#> 1012                   Patrick Hand SC    400 <NA> google
#> 1013                           Pattaya    400 <NA> google
#> 1014                         Patua One    400 <NA> google
#> 1015                           Pavanam    400 <NA> google
#> 1016                       Paytone One    400 <NA> google
#> 1017                           Peddana    400 <NA> google
#> 1018                           Peralta    400 <NA> google
#> 1019                  Permanent Marker    400 <NA> google
#> 1020                          Petemoss    400 <NA> google
#> 1021               Petit Formal Script    400 <NA> google
#> 1022                           Petrona    400 <NA> google
#> 1023                       Philosopher    400 <NA> google
#> 1024                         Piazzolla    400 <NA> google
#> 1025                            Piedra    400 <NA> google
#> 1026                     Pinyon Script    400 <NA> google
#> 1027                        Pirata One    400 <NA> google
#> 1028                           Plaster    400 <NA> google
#> 1029                              Play    400 <NA> google
#> 1030                          Playball    400 <NA> google
#> 1031                  Playfair Display    400 <NA> google
#> 1032               Playfair Display SC    400 <NA> google
#> 1033                           Podkova    400 <NA> google
#> 1034                        Poiret One    400 <NA> google
#> 1035                        Poller One    400 <NA> google
#> 1036                              Poly    400 <NA> google
#> 1037                          Pompiere    400 <NA> google
#> 1038                      Pontano Sans    400 <NA> google
#> 1039                        Poor Story    400 <NA> google
#> 1040                           Poppins    400 <NA> google
#> 1041                  Port Lligat Sans    400 <NA> google
#> 1042                  Port Lligat Slab    400 <NA> google
#> 1043                         Potta One    400 <NA> google
#> 1044                    Pragati Narrow    400 <NA> google
#> 1045                            Praise    400 <NA> google
#> 1046                             Prata    400 <NA> google
#> 1047                       Preahvihear    400 <NA> google
#> 1048                    Press Start 2P    400 <NA> google
#> 1049                             Pridi    400 <NA> google
#> 1050                    Princess Sofia    400 <NA> google
#> 1051                          Prociono    400 <NA> google
#> 1052                            Prompt    400 <NA> google
#> 1053                        Prosto One    400 <NA> google
#> 1054                       Proza Libre    400 <NA> google
#> 1055                       Public Sans    400 <NA> google
#> 1056                      Puppies Play    400 <NA> google
#> 1057                           Puritan    400 <NA> google
#> 1058                      Purple Purse    400 <NA> google
#> 1059                            Qahiri    400 <NA> google
#> 1060                            Quando    400 <NA> google
#> 1061                          Quantico    400 <NA> google
#> 1062                      Quattrocento    400 <NA> google
#> 1063                 Quattrocento Sans    400 <NA> google
#> 1064                         Questrial    400 <NA> google
#> 1065                         Quicksand    400 <NA> google
#> 1066                    Quintessential    400 <NA> google
#> 1067                           Qwigley    400 <NA> google
#> 1068                   Qwitcher Grypen    400 <NA> google
#> 1069                   Racing Sans One    400 <NA> google
#> 1070                            Radley    400 <NA> google
#> 1071                          Rajdhani    400 <NA> google
#> 1072                            Rakkas    400 <NA> google
#> 1073                           Raleway    400 <NA> google
#> 1074                      Raleway Dots    400 <NA> google
#> 1075                        Ramabhadra    400 <NA> google
#> 1076                          Ramaraja    400 <NA> google
#> 1077                            Rambla    400 <NA> google
#> 1078                      Rammetto One    400 <NA> google
#> 1079                       Rampart One    400 <NA> google
#> 1080                          Ranchers    400 <NA> google
#> 1081                            Rancho    400 <NA> google
#> 1082                             Ranga    400 <NA> google
#> 1083                              Rasa    400 <NA> google
#> 1084                         Rationale    400 <NA> google
#> 1085                      Ravi Prakash    400 <NA> google
#> 1086                        Readex Pro    400 <NA> google
#> 1087                         Recursive    400 <NA> google
#> 1088                   Red Hat Display    400 <NA> google
#> 1089                      Red Hat Mono    400 <NA> google
#> 1090                      Red Hat Text    400 <NA> google
#> 1091                          Red Rose    400 <NA> google
#> 1092                          Redacted    400 <NA> google
#> 1093                   Redacted Script    400 <NA> google
#> 1094                         Redressed    400 <NA> google
#> 1095                         Reem Kufi    400 <NA> google
#> 1096                     Reenie Beanie    400 <NA> google
#> 1097                        Reggae One    400 <NA> google
#> 1098                           Revalia    400 <NA> google
#> 1099                     Rhodium Libre    400 <NA> google
#> 1100                            Ribeye    400 <NA> google
#> 1101                     Ribeye Marrow    400 <NA> google
#> 1102                         Righteous    400 <NA> google
#> 1103                            Risque    400 <NA> google
#> 1104                         Road Rage    400 <NA> google
#> 1105                            Roboto    400 <NA> google
#> 1106                  Roboto Condensed    400 <NA> google
#> 1107                       Roboto Mono    400 <NA> google
#> 1108                      Roboto Serif    400 <NA> google
#> 1109                       Roboto Slab    400 <NA> google
#> 1110                         Rochester    400 <NA> google
#> 1111                           Rock 3D    400 <NA> google
#> 1112                         Rock Salt    400 <NA> google
#> 1113                     RocknRoll One    400 <NA> google
#> 1114                           Rokkitt    400 <NA> google
#> 1115                         Romanesco    400 <NA> google
#> 1116                         Ropa Sans    400 <NA> google
#> 1117                           Rosario    400 <NA> google
#> 1118                          Rosarivo    400 <NA> google
#> 1119                      Rouge Script    400 <NA> google
#> 1120                           Rowdies    400 <NA> google
#> 1121                         Rozha One    400 <NA> google
#> 1122                             Rubik    400 <NA> google
#> 1123                     Rubik Beastly    400 <NA> google
#> 1124                    Rubik Mono One    400 <NA> google
#> 1125                              Ruda    400 <NA> google
#> 1126                            Rufina    400 <NA> google
#> 1127                       Ruge Boogie    400 <NA> google
#> 1128                            Ruluko    400 <NA> google
#> 1129                        Rum Raisin    400 <NA> google
#> 1130                    Ruslan Display    400 <NA> google
#> 1131                         Russo One    400 <NA> google
#> 1132                            Ruthie    400 <NA> google
#> 1133                               Rye    400 <NA> google
#> 1134                     STIX Two Text    400 <NA> google
#> 1135                        Sacramento    400 <NA> google
#> 1136                           Sahitya    400 <NA> google
#> 1137                              Sail    400 <NA> google
#> 1138                             Saira    400 <NA> google
#> 1139                   Saira Condensed    400 <NA> google
#> 1140             Saira Extra Condensed    400 <NA> google
#> 1141              Saira Semi Condensed    400 <NA> google
#> 1142                 Saira Stencil One    400 <NA> google
#> 1143                             Salsa    400 <NA> google
#> 1144                           Sanchez    400 <NA> google
#> 1145                          Sancreek    400 <NA> google
#> 1146                           Sansita    400 <NA> google
#> 1147                   Sansita Swashed    400 <NA> google
#> 1148                           Sarabun    400 <NA> google
#> 1149                            Sarala    400 <NA> google
#> 1150                            Sarina    400 <NA> google
#> 1151                          Sarpanch    400 <NA> google
#> 1152                       Sassy Frass    400 <NA> google
#> 1153                           Satisfy    400 <NA> google
#> 1154                   Sawarabi Gothic    400 <NA> google
#> 1155                   Sawarabi Mincho    400 <NA> google
#> 1156                             Scada    400 <NA> google
#> 1157                  Scheherazade New    400 <NA> google
#> 1158                        Schoolbell    400 <NA> google
#> 1159                         Scope One    400 <NA> google
#> 1160                    Seaweed Script    400 <NA> google
#> 1161                       Secular One    400 <NA> google
#> 1162                      Sedgwick Ave    400 <NA> google
#> 1163              Sedgwick Ave Display    400 <NA> google
#> 1164                               Sen    400 <NA> google
#> 1165                         Sevillana    400 <NA> google
#> 1166                       Seymour One    400 <NA> google
#> 1167                Shadows Into Light    400 <NA> google
#> 1168            Shadows Into Light Two    400 <NA> google
#> 1169                          Shalimar    400 <NA> google
#> 1170                            Shanti    400 <NA> google
#> 1171                             Share    400 <NA> google
#> 1172                        Share Tech    400 <NA> google
#> 1173                   Share Tech Mono    400 <NA> google
#> 1174                  Shippori Antique    400 <NA> google
#> 1175               Shippori Antique B1    400 <NA> google
#> 1176                   Shippori Mincho    400 <NA> google
#> 1177                Shippori Mincho B1    400 <NA> google
#> 1178                           Shizuru    400 <NA> google
#> 1179                         Shojumaru    400 <NA> google
#> 1180                       Short Stack    400 <NA> google
#> 1181                         Shrikhand    400 <NA> google
#> 1182                          Siemreap    400 <NA> google
#> 1183                        Sigmar One    400 <NA> google
#> 1184                           Signika    400 <NA> google
#> 1185                  Signika Negative    400 <NA> google
#> 1186                         Simonetta    400 <NA> google
#> 1187                        Single Day    400 <NA> google
#> 1188                           Sintony    400 <NA> google
#> 1189                     Sirin Stencil    400 <NA> google
#> 1190                          Six Caps    400 <NA> google
#> 1191                           Skranji    400 <NA> google
#> 1192                        Slabo 13px    400 <NA> google
#> 1193                        Slabo 27px    400 <NA> google
#> 1194                           Slackey    400 <NA> google
#> 1195                            Smokum    400 <NA> google
#> 1196                            Smooch    400 <NA> google
#> 1197                       Smooch Sans    400 <NA> google
#> 1198                            Smythe    400 <NA> google
#> 1199                           Sniglet    400 <NA> google
#> 1200                           Snippet    400 <NA> google
#> 1201                     Snowburst One    400 <NA> google
#> 1202                        Sofadi One    400 <NA> google
#> 1203                             Sofia    400 <NA> google
#> 1204                            Solway    400 <NA> google
#> 1205                        Song Myung    400 <NA> google
#> 1206                        Sonsie One    400 <NA> google
#> 1207                              Sora    400 <NA> google
#> 1208                  Sorts Mill Goudy    400 <NA> google
#> 1209                   Source Code Pro    400 <NA> google
#> 1210                     Source Sans 3    400 <NA> google
#> 1211                   Source Sans Pro    400 <NA> google
#> 1212                    Source Serif 4    400 <NA> google
#> 1213                  Source Serif Pro    400 <NA> google
#> 1214                     Space Grotesk    400 <NA> google
#> 1215                        Space Mono    400 <NA> google
#> 1216                           Spartan    400 <NA> google
#> 1217                     Special Elite    400 <NA> google
#> 1218                          Spectral    400 <NA> google
#> 1219                       Spectral SC    400 <NA> google
#> 1220                        Spicy Rice    400 <NA> google
#> 1221                         Spinnaker    400 <NA> google
#> 1222                            Spirax    400 <NA> google
#> 1223                       Spline Sans    400 <NA> google
#> 1224                        Squada One    400 <NA> google
#> 1225              Sree Krushnadevaraya    400 <NA> google
#> 1226                          Sriracha    400 <NA> google
#> 1227                          Srisakdi    400 <NA> google
#> 1228                       Staatliches    400 <NA> google
#> 1229                         Stalemate    400 <NA> google
#> 1230                     Stalinist One    400 <NA> google
#> 1231                   Stardos Stencil    400 <NA> google
#> 1232                             Stick    400 <NA> google
#> 1233                    Stick No Bills    400 <NA> google
#> 1234             Stint Ultra Condensed    400 <NA> google
#> 1235              Stint Ultra Expanded    400 <NA> google
#> 1236                             Stoke    400 <NA> google
#> 1237                            Strait    400 <NA> google
#> 1238                      Style Script    400 <NA> google
#> 1239                           Stylish    400 <NA> google
#> 1240               Sue Ellen Francisco    400 <NA> google
#> 1241                          Suez One    400 <NA> google
#> 1242                     Sulphur Point    400 <NA> google
#> 1243                            Sumana    400 <NA> google
#> 1244                         Sunflower    400 <NA> google
#> 1245                         Sunshiney    400 <NA> google
#> 1246                  Supermercado One    400 <NA> google
#> 1247                              Sura    400 <NA> google
#> 1248                           Suranna    400 <NA> google
#> 1249                         Suravaram    400 <NA> google
#> 1250                       Suwannaphum    400 <NA> google
#> 1251                Swanky and Moo Moo    400 <NA> google
#> 1252                         Syncopate    400 <NA> google
#> 1253                              Syne    400 <NA> google
#> 1254                         Syne Mono    400 <NA> google
#> 1255                      Syne Tactile    400 <NA> google
#> 1256                           Tajawal    400 <NA> google
#> 1257                         Tangerine    400 <NA> google
#> 1258                            Taprom    400 <NA> google
#> 1259                             Tauri    400 <NA> google
#> 1260                           Taviraj    400 <NA> google
#> 1261                              Teko    400 <NA> google
#> 1262                             Telex    400 <NA> google
#> 1263                Tenali Ramakrishna    400 <NA> google
#> 1264                        Tenor Sans    400 <NA> google
#> 1265                       Text Me One    400 <NA> google
#> 1266                         Texturina    400 <NA> google
#> 1267                         Thasadith    400 <NA> google
#> 1268                The Girl Next Door    400 <NA> google
#> 1269                      The Nautigal    400 <NA> google
#> 1270                            Tienne    400 <NA> google
#> 1271                           Tillana    400 <NA> google
#> 1272                           Timmana    400 <NA> google
#> 1273                             Tinos    400 <NA> google
#> 1274                         Titan One    400 <NA> google
#> 1275                     Titillium Web    400 <NA> google
#> 1276                          Tomorrow    400 <NA> google
#> 1277                           Tourney    400 <NA> google
#> 1278                       Trade Winds    400 <NA> google
#> 1279                         Train One    400 <NA> google
#> 1280                           Trirong    400 <NA> google
#> 1281                          Trispace    400 <NA> google
#> 1282                           Trocchi    400 <NA> google
#> 1283                           Trochut    400 <NA> google
#> 1284                        Truculenta    400 <NA> google
#> 1285                           Trykker    400 <NA> google
#> 1286                        Tulpen One    400 <NA> google
#> 1287                       Turret Road    400 <NA> google
#> 1288                      Twinkle Star    400 <NA> google
#> 1289                            Ubuntu    400 <NA> google
#> 1290                  Ubuntu Condensed    400 <NA> google
#> 1291                       Ubuntu Mono    400 <NA> google
#> 1292                             Uchen    400 <NA> google
#> 1293                             Ultra    400 <NA> google
#> 1294                    Uncial Antiqua    400 <NA> google
#> 1295                          Underdog    400 <NA> google
#> 1296                         Unica One    400 <NA> google
#> 1297                    UnifrakturCook    400 <NA> google
#> 1298                UnifrakturMaguntia    400 <NA> google
#> 1299                           Unkempt    400 <NA> google
#> 1300                            Unlock    400 <NA> google
#> 1301                              Unna    400 <NA> google
#> 1302                          Urbanist    400 <NA> google
#> 1303                             VT323    400 <NA> google
#> 1304                       Vampiro One    400 <NA> google
#> 1305                            Varela    400 <NA> google
#> 1306                      Varela Round    400 <NA> google
#> 1307                             Varta    400 <NA> google
#> 1308                       Vast Shadow    400 <NA> google
#> 1309                      Vesper Libre    400 <NA> google
#> 1310                      Viaoda Libre    400 <NA> google
#> 1311                             Vibes    400 <NA> google
#> 1312                             Vibur    400 <NA> google
#> 1313                          Vidaloka    400 <NA> google
#> 1314                              Viga    400 <NA> google
#> 1315                             Voces    400 <NA> google
#> 1316                           Volkhov    400 <NA> google
#> 1317                          Vollkorn    400 <NA> google
#> 1318                       Vollkorn SC    400 <NA> google
#> 1319                          Voltaire    400 <NA> google
#> 1320                   Vujahday Script    400 <NA> google
#> 1321           Waiting for the Sunrise    400 <NA> google
#> 1322                          Wallpoet    400 <NA> google
#> 1323                   Walter Turncoat    400 <NA> google
#> 1324                            Warnes    400 <NA> google
#> 1325                         Waterfall    400 <NA> google
#> 1326                         Wellfleet    400 <NA> google
#> 1327                         Wendy One    400 <NA> google
#> 1328                          WindSong    400 <NA> google
#> 1329                          Wire One    400 <NA> google
#> 1330                         Work Sans    400 <NA> google
#> 1331                         Xanh Mono    400 <NA> google
#> 1332                           Yaldevi    400 <NA> google
#> 1333                 Yanone Kaffeesatz    400 <NA> google
#> 1334                       Yantramanav    400 <NA> google
#> 1335                         Yatra One    400 <NA> google
#> 1336                        Yellowtail    400 <NA> google
#> 1337                         Yeon Sung    400 <NA> google
#> 1338                        Yeseva One    400 <NA> google
#> 1339                        Yesteryear    400 <NA> google
#> 1340                            Yomogi    400 <NA> google
#> 1341                              Yrsa    400 <NA> google
#> 1342                         Yuji Boku    400 <NA> google
#> 1343             Yuji Hentaigana Akari    400 <NA> google
#> 1344           Yuji Hentaigana Akebono    400 <NA> google
#> 1345                          Yuji Mai    400 <NA> google
#> 1346                        Yuji Syuku    400 <NA> google
#> 1347                       Yusei Magic    400 <NA> google
#> 1348                      ZCOOL KuaiLe    400 <NA> google
#> 1349             ZCOOL QingKe HuangYou    400 <NA> google
#> 1350                     ZCOOL XiaoWei    400 <NA> google
#> 1351                       Zen Antique    400 <NA> google
#> 1352                  Zen Antique Soft    400 <NA> google
#> 1353                          Zen Dots    400 <NA> google
#> 1354           Zen Kaku Gothic Antique    400 <NA> google
#> 1355               Zen Kaku Gothic New    400 <NA> google
#> 1356                     Zen Kurenaido    400 <NA> google
#> 1357                          Zen Loop    400 <NA> google
#> 1358                   Zen Maru Gothic    400 <NA> google
#> 1359                    Zen Old Mincho    400 <NA> google
#> 1360                     Zen Tokyo Zoo    400 <NA> google
#> 1361                            Zeyada    400 <NA> google
#> 1362                     Zhi Mang Xing    400 <NA> google
#> 1363                        Zilla Slab    400 <NA> google
#> 1364              Zilla Slab Highlight    400 <NA> google
# }
```
