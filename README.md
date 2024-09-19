# IZB_scripts
Een collectie handige scripts voor de dagelijkse vragen binnen IZB.

# Aanwezige scripts

## IZB dashboard
De databasebestanden en een voorbeeldindeling van de agents/infections/diagnoses in HPZone zijn hier te vinden. Tevens is er een omschrijving van de dataverwerking en een verwerkingsscript toegevoegd. Hiermee is vrij snel een IZB dashboard op te zetten.

## data_nivel.R
Het Nivel publiceert wekelijks cijfers van de eerstelijns zorgregistraties per GGD in PDF-formaat. Dit script zet deze publicaties om in bruikbare tabellen binnen R. Let op: voor de opslag wordt een interne onderzoeksdatabase gebruikt; de datastructuur is los toegevoegd in data_nivel.sql.

## dierdata.Rmd
Op basis van de dierdata van het RIVM produceert dit bestand een interactieve kaart, waarop ingezoomd kan worden tot op bedrijfsniveau per type dier. Hiermee is een regionaal overzicht van de veehouderijen inzichtelijk te maken.
![afbeelding](https://github.com/user-attachments/assets/74a15bde-7ab2-4cbe-b5ce-2eea4c1de6fe)


## download bevolking.R
De tabel met inwonersaantallen was in augustus en september 2023 niet meer direct op te vragen via de OData-feed van het CBS. Deze kon toen alleen in zijn geheel worden gedownload. Dit is veel te groot voor dagelijks gebruik, waardoor een kleine export handiger is. Deze kan worden gemaakt met dit script. Uitvoeren is echter bijzonder intensief, waardoor de meeste GGD-computers deze niet uit kunnen voeren. Het is dan ook alleen als laatste redmiddel bedoeld.

## overzicht inwoners IZB.R
Dit script verzamelt een grote hoeveelheid data uit openbare bronnen en combineert deze informatie tot een (redelijk) leesbare Excelsheet per gemeente en per regio. Let op: enkele datasets bij het CBS en RIVM worden niet per jaar aangepast, maar worden opnieuw aangemaakt. Het kan daardoor nodig zijn om voor een volgende versie enige tabelnamen aan te passen.
![afbeelding](https://github.com/ggdatascience/IZB_scripts/assets/125073330/18965456-02f4-4cfb-998a-bf0fc2635623)

## kaarten_IZB.R
Voor het overzicht van de regio is het soms handiger om kaarten te maken met een visuele weergave van bijvoorbeeld grote bedrijven in de regio. Dit script maakt een collectie aan handige kaarten en overzichten, welke gebruikt kunnen worden voor een globaal overzicht of voor het inzoomen op een bepaalde regio. **Let op:** er wordt gebruik gemaakt van lokale datasets, zoals bijvoorbeeld een datadump van het KvK-register, dus niet alle stukken zullen werken bij een andere GGD. Er zitten echter ook algemene kaarten tussen, die wellicht interessant zijn voor een andere GGD.

## interactieve kaart IZB.R
In navolging van de kaartjes die met kaarten_IZB.R kunnen worden gemaakt gingen we veel meer kaarten maken. Hierbij bleek een interactieve versie veel praktischer; het aantal bedrijven in één GGD-regio is bijvoorbeeld niet goed weer te geven op een statische kaart. Dit script maakt een interactieve (zoombaar, klikbaar) kaart van een fiks aantal databronnen, waaronder koeltorens, zorgkosten, zorgverleners, agrarische bedrijven, enz. Deze zijn over elkaar heen te leggen indien gewenst. Verder kan op een marker geklikt worden voor meer informatie.
![afbeelding](https://github.com/ggdatascience/IZB_scripts/assets/125073330/08f73eaa-48db-4ace-abc3-f7c884635d0b)

## vaccinatiegraad.R
Er wordt regelmatig gevraagd om grafieken van de vaccinatiegraad in een bepaalde gemeente of van een bepaalde vaccinatie. Dit script maakt een hele stapel grafieken (per gemeente, per type, kaartjes, enz.) voor algemeen gebruik.
![afbeelding](https://github.com/ggdatascience/IZB_scripts/assets/125073330/08c6eba7-f6da-4ee4-b321-35a1efc1eb1c)

## vaccinatiegraad per pc4 en wijk.R
Vanuit het RIVM worden jaarlijks vaccinatiecijfers per postcode 4 gedeeld. Dit script maakt een aantal handige weergaven van deze cijfers, waaronder een zoombare kaart met postcodes en percentages, verloop over jaren, en een kaart met het verschil tussen de laatste twee jaar.

## veehouderijen_grafieken.R
Om overzicht te krijgen in de verhouding tussen bevolkingsdichtheid en dierenaantallen produceert dit script per type dier een kaart die het totale aantal en het relatieve aantal dieren weergeeft.
![bevolkingsdichtheid NL](https://github.com/user-attachments/assets/266b9c9a-ebfd-4c80-bb96-681f69212a97)
![NL_Rundvee](https://github.com/user-attachments/assets/787d0f58-6436-4ad3-b065-2386d6c08102)


