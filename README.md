# IZB_scripts
Een collectie handige scripts voor de dagelijkse vragen binnen IZB.

# Aanwezige scripts

## download bevolking.R
De tabel met inwonersaantallen is sinds augustus 2023 niet meer direct op te vragen via de OData-feed van het CBS. Deze kan sindsdien alleen in zijn geheel worden gedownload. Dit is veel te groot voor dagelijks gebruik, waardoor een kleine export handiger is. Deze kan worden gemaakt met dit script.

## overzicht inwoners IZB.R
Dit script verzamelt een grote hoeveelheid data uit openbare bronnen en combineert deze informatie tot een (redelijk) leesbare Excelsheet per gemeente en per regio. Let op: enkele datasets bij het CBS en RIVM worden niet per jaar aangepast, maar worden opnieuw aangemaakt. Het kan daardoor nodig zijn om voor een volgende versie enige tabelnamen aan te passen.

## kaarten_IZB.R
Voor het overzicht van de regio is het soms handiger om kaarten te maken met een visuele weergave van bijvoorbeeld grote bedrijven in de regio. Dit script maakt een collectie aan handige kaarten en overzichten, welke gebruikt kunnen worden voor een globaal overzicht of voor het inzoomen op een bepaalde regio. **Let op:** er wordt gebruik gemaakt van lokale datasets, zoals bijvoorbeeld een datadump van het KvK-register, dus niet alle stukken zullen werken bij een andere GGD. Er zitten echter ook algemene kaarten tussen, die wellicht interessant zijn voor een andere GGD.
