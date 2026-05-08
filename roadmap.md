
# Roadmap

## To do

- pd_poly_manage_overlaps ... should it be a hidden function? ... it is called by the pd_sanitize function
    * YES! Vi gør den skjult for brugerne
- i pd_json2pd .. skal vi flippe y aksen? skal det være en option?
    * YES! Vi gør det til en funktions parameter med default = TRUE (flip)
- kunne pd_recreate_drawing plotte alle på samme baggrund i stedet for flere facetter?
    * NO! ...funktionen hedder 're-create' .. skal derfor lave original indtastningen
- vi skal kigge nærmere ind i pd_json2pd funktionen med nogle ægte PainSketchR data -- 's' kolonnen, default w og h værdier?
- ændre pd_spray_trim til pd_png_trim
    * YES! Omdøb til pdr_rgba_trim
- i pd_png_trim funktionen kan vi droppe alt om fil-indlæsning og i stedet bruge .png kolonner genereret af Steens pd_to_png funktionen
    * YES! Det skal SON fikse og husk: OBS navne ændret til rgba
- i pd_png_trim funktionen skal vi indføre en parameter i funktionskaldet 'col_name' som definerer kolonne navnet i pd data hvor den nye (trimmede) png data indsættes
    * NO! Vi laver samme funktionalitet som i pdr_sanitize, hvor den kan håndtere kolonne ṕ' OG et helt vaid pd tibble
- i sammenfattende funktioner (f.eks pd_anatomy_merge) hvor et antal tegninger/række bliver 'summarized' til én skal vi sætte 's' kolonnen til fornuftige default værdier
    * YES! Det har SON lavet -- skal lige tjekkes så ned i DONE
- hvordan skal vi håndtere NA værdier -- f.eks hvis 's' og 'p' er NA? (pdr_check_data)
    * NA værdier er acceptable i kolonnerne s, p, coord, ts, app
    * NA værdier er IKKE acceptable i kolonnerne id, w, h
- pd_mird2pd skal generere fornuftige (og korrekte) default værdier for w, h, coord, etc
    * YES! Det skal vi bare have Natalie til at hjælpe med ... de rigtige værdier indsættes i import funktionen
- Tjek korrekt brug af inst/extdata til eksterne data sæt som brugeren kan læse ind med import funktionerne
    * SON !
- pd_demodata skal indeholde to kategoriske og 1 kontinuerlig variabel til demonstration af pd_create_heatmap
    * SON ! F.eks Group: A|B|C, Treatment: Active|Placebo og VAS: (..en moduleret pd area)
- webpage
    * Hvornår er vores dokumentation god nok til at prøve at generere en info page?
- gitlab SDU?
    * Skal vi flytte repo til gitlab MAYBE! Det tænker SFH over ....
- alle funktioner nødvendige?
    * SON + SFH kigger lige funktions listen igennem -- har vi det (og kun det) vi skal bruge?


## Done

- SON: pdr_check_data skal tjekke indholdet af p og s, højde og bredde m.m.
- SON: Warning hvis flere farver 
- SH: summary af .png (intensitet / alpha)
- SH: ændret pd_add_png() til pd_to_png() den virker nu via mutate(). pd_add_png() er fjernet
- SH: ændret pd_alpha_intensity() og pd_alpha_area() således at de nu vikrer via mutate() uden at bruger skal wrappe med map_dbl()
- SH: - kan png filer skabes i ram uden at skrive til disk? Ja - indført som default
- SH: - skal a i kolonne s i pain drawing data struktur være 0:1 eller 0:255 eller 0:127 Yes - er 0:255