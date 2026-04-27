
# Roadmap

## To do

- pd_poly_manage_overlaps ... should it be a hidden function? ... it is called by the pd_sanitize function
- i pd_json2pd .. skal vi flippe y aksen? skal det være en option?
- kunne pd_recreate_drawing plotte alle på samme baggrund i stedet for flere facetter?
    - Hvorfor?
    - Ja, vi kan lave et argument "facet" som defaulter til TRUE, hvis ikke så bliver alle plottet oven i hinanden

- vi skal kigge nærmere ind i pd_json2pd funktionen med nogle ægte PainSketchR data -- 's' kolonnen, default w og h værdier?
- ændre pd_spray_trim til pd_png_trim
- i pd_png_trim : gange alpha med alpha kanal
- skal a i kolonne s i pain drawing data struktur være 0:1 eller 0:255 eller 0:127
    - Jeg tror painsketch leverer i 0:255
- i pd_png_trim funktionen kan vi droppe alt om fil-indlæsning og i stedet bruge .png kolonner genereret af Steens pd_to_png funktionen
    - OBS navne ændret til rgba
- i pd_png_trim funktionen skal vi indføre en parameter i funktionskaldet 'col_name' som definerer kolonne navnet i pd data hvor den nye (trimmede) png data indsættes
- i sammenfattende funktioner (f.eks pd_anatomy_merge) hvor et antal tegninger/række bliver 'summarized' til én skal vi sætte 's' kolonnen til fornuftige default værdier
- hvordan skal vi håndtere NA værdier -- f.eks hvis 's' og 'p' er NA? (pd_check_data)
- pd_mird2pd skal generere fornuftige (og korrekte) default værdier for w, h, coord, etc
- i DATASET.R ..  brug pd_mird2pd til at indlæse demo_data
- pd_demodata skal indeholde to kategoriske og 1 kontinuerlig variabel til demonstration af pd_create_heatmap


- webpage
- gitlab SDU?

## Done

- SON: pd_check_data skal tjekke indholdet af p og s, højde og bredde m.m.
- SON: Warning hvis flere farver 
- SH: summary af .png (intensitet / alpha)
- SH: ændret pd_add_png() til pd_to_png() den virker nu via mutate(). pd_add_png() er fjernet
- SH: ændret pd_alpha_intensity() og pd_alpha_area() således at de nu vikrer via mutate() uden at bruger skal wrappe med map_dbl()
- SH: - kan png filer skabes i ram uden at skrive til disk? Ja - indført som default