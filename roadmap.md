
# Roadmap

## To do

- ændre pd_spray_trim til pd_png_trim
- i pd_png_trim : gange alpha med alpha kanal
- i pd_png_trim funktionen kan vi droppe alt om fil-indlæsning og i stedet bruge .png kolonner genereret af Steens pd_to_png funktionen
- i pd_png_trim funktionen skal vi indføre en parameter i funktionskaldet 'col_name' som definerer kolonne navnet i pd data hvor den nye (trimmede) png data indsættes
- i sammenfattende funktioner (f.eks pd_anatomy_merge) hvor et antal tegninger/række bliver 'summarized' til én skal vi sætte 's' kolonnen til fornuftige default værdier

- kan png filer skabes i ram uden at skrive til disk?
- hvordan skal vi håndtere NA værdier -- f.eks hvis 's' og 'p' er NA? (pd_check_data)
- pd_mird2pd skal generere fornuftige (og korrekte) default værdier for w, h, coord, etc

- webpage
- gitlab SDU?

## Done

- SON: pd_check_data skal tjekke indholdet af p og s, højde og bredde m.m.
- SON: Warning hvis flere farver 
- SH: summary af .png (intensitet / alpha)
- SH: ændret pd_add_png() til pd_to_png() den virker nu via mutate(). pd_add_png() er fjernet
- SH: ændret pd_alpha_intensity() og pd_alpha_area() således at de nu vikrer via mutate() uden at bruger skal wrappe med map_dbl()