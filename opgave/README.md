# Opgave Project Logisch programmeren

De opgave van dit jaar bestaat uit het implementeren van een schaakcomputer. In
dit document geven we een overzicht van de basis functionaliteit van deze
schaakcomputer. Verder geven we de vereisten waaraan je project moet voldoen.
Studenten worden aangemoedigd om zelf extra functionaliteit te voorzien.

## Basis API van de schaakcomputer

Je schaakcomputer moet een programma zijn dat een unicode schaakbord
binnenkrijgt via standaard invoer en een unicode schaakbord naar de standaard
output schrijft. Het bord aan de standaard uitvoer is het resultaat van de beste
zet volgens je schaakcomputer. Je mag je dus beperken tot het implementeren van
de schaakcomputer als een input-output programma, je hoeft je niet bezig te
houden met het ontwikkelen van een GUI voor het programma.

Je programma moet een zet kunnen vinden binnen "redelijke tijd", gemiddeld
maximum 30 seconden per zet (harde limit van 120 seconden voor 1 zet).

## Testen van de schaakcomputer

Een basistest van je schaakcomputer is hem te laten spelen tegen een random
speler. We voorzien in de upstream opgave repo code om je schaakcomputer een
spel te laten spelen tegen een random speler (`opgave/vsRandom`). Die testcode
gaat tijdens het spelen ook na of je schaakcomputer steeds legale zetten maakt.
Indien je schaakcomputer een illegale zet maakt zul je hiervan op de hoogte
gesteld worden. Schaakcomputers die niet in staat zijn om tegen deze random
speler te spelen zullen niet aanvaard worden.

Je programma moet ook een test modus hebben waarbij alle mogelijke volgende
borden naar standaard uitvoer worden geschreven, gescheiden door `"\n~\n"`. Deze
modus wordt geactiveerd als je het programma opstart met een extra argument
namelijk `TEST`. Deze functionaliteit zal gebruikt worden om je project
automatisch te evalueren. We verwachten dat je ook zelf testcode maakt om je
schaakcomputer te testen (je mag hiervoor vertrekken van de code op in de
upstream opgave repo).

Met de "driemaal dezelfde stelling", de "50 zetten regel" en de "75 halve
zetten" regel hoef je geen rekening te houden.

## Oproepen van je schaakcomputer

Je schaakcomputer moet werken als ze wordt uitgevoerd met volgende opdracht:

```
cat inputBord | swipl -f -q -t "halt(1)" -O main.pl
```

Waarbij `inputBord` een utf8 unicode tekstbestand is met daarin de "Unicode
Schaakbord Notatie" van een input bord (zie verder).

De test functie zal worden opgeroepen met:

```
cat inputBord | swipl -f -q -t "halt(1)" -O main.pl TEST
```

## Niet functionele eisen

Naast de basisfunctionaliteit vragen we enkele niet functionele eisen waar je
project aan dient te voldoen. Deze niet functionele eisen zijn even belangrijk
als de functionele eisen van het project.

- De code moet goed gedocumenteerd zijn, er moet commentaar geschreven zijn bij
  praktisch elk predicaat.
- Je code moet getest zijn, dit wil zeggen dat je voor elk van de bewegingen
  zelf een test schrijft zodat je zeker bent dat de basis functionaliteit werkt.
  Gebruik hiervoor bijvoorbeeld PLUnit.
- Je schaakcomputer moet gebaseerd zijn op een variant van min-max bomen
  (alpha-bèta snoeien). We laten uitzonderingen toe op deze regel in onderling
  overleg met de assistent van het vak.
- Je schaakcomputer moet testbaar zijn met de voorziene code in de `opgave` map
  van de upstream opgave repository. Projecten die niet testbaar zijn met deze
  automatische test zullen automatisch als onontvankelijk verklaard worden.

## Verslag

We verwachten een _bondig_ verslag die de algemene oplossingsstrategie van je
schaakcomputer beschrijft. Voeg aan je verslag je code toe met lijnnummers zodat
je in de uitleg van je verslag kan verwijzen naar de relevante delen van je
code. Je bent zelf vrij hoe je dit verslag organiseert maar we verwachten op
zijn minst de volgende onderdelen:

- Inleiding
- Interne Bord voorstelling
- Algoritme (met kort voorbeeld)
- Benchmarks (voor verschillende recursiedieptes)
- Conclusie (wat heb je gerealiseerd en wat kan beter)

## Checklist

Je project is veel meer dan enkel de code van de schaakcomputer, hieronder een
checklist om na te gaan of je alle onderdelen hebt afgewerkt.

- [ ] Schaakcomputer
- [ ] Code documentatie
- [ ] Testcode
- [ ] Test functionaliteit (`TEST` argument)
- [ ] Verslag
- [ ] Alles pushen naar `master` op
      `git@subgit.ugent.be:2020-2021/LP/{studentnr}`

## Basis regels

De regels van schaken hebben we voor jullie hieronder nog eens overgenomen van
[Wikipedia](https://nl.wikipedia.org/w/index.php?title=Schaken&oldid=51233822#Spelregels).

Schaken heeft een aantal basis regels die jou schaakcomputer moet respecteren.
Op elk veld van het schaakbord mag hoogstens 1 stuk staan. Door een eigen stuk
te spelen naar een veld waarop een stuk van de tegenstander staat, wordt dat
stuk geslagen en van het bord genomen, het slaande stuk komt in de plaats. Met
uitzondering van het paard bewegen alle stukken volgens een rechte lijn waarbij
er geen stukken op niet-eindpunten van de beweging zijn. Met andere woorden,
enkel het paard kan over andere stukken "springen".

- Een koning neemt één stap tegelijk, recht of diagonaal. Daarnaast heeft de
  koning eenmaal per partij een speciale mogelijkheid, zie korte en lange
  rokade.
- Een dame mag naar keuze horizontaal, verticaal of diagonaal bewegen.
- Een toren mag naar keuze horizontaal of verticaal bewegen (als de loop niet
  door andere stukken geblokkeerd wordt).
- Een loper mag diagonaal bewegen.
- Een paard beweegt door eerst 2 velden horizontaal of verticaal en daarna 1
  veld in een richting loodrecht daarop. Een paard kan in tegenstelling over de
  andere stukken ‘springen’.
- Een pion kan uitsluitend recht vooruit gaan als het veld recht vooruit leeg
  is, en hij kan uitsluitend schuin vooruit gaan als hij daarmee een stuk slaat.
  Vanuit zijn begin positie (rij 2 of 7) heeft een pion ook de mogelijkheid om
  twee vakken vooruit te gaan.

Het doel van het spel is het schaakmat zetten van de koning van de tegenpartij.
Dit is het geval als de tegenstander aan beurt is, schaak staat en geen geldige
zet heeft om dat op te heffen. De partij is hiermee onmiddellijk afgelopen; de
koning wordt dus niet daadwerkelijk geslagen.

Staat de eigen koning niet schaak en is er geen enkele reglementaire zet
voorhanden, dan is een patstelling ontstaan, of kortweg pat, en eindigt de
partij onmiddellijk in remise (gelijkspel).

![Beginopstelling](board.svg)

![Beweegrichtingen stukken](moves.svg)

### Korte en lange rokade

Men onderscheidt de korte en de lange rokade. In beide gevallen gaat de koning
vanuit zijn beginpositie (op de e-lijn) twee velden opzij en gaat de toren
vanuit zijn beginpositie over de koning heen naar het veld direct naast de
koning. Bij de korte rokade gaat de koning dus naar de g-lijn en de toren van de
h- naar de f-lijn. De toren springt daarbij als het ware over de koning heen.
Bij de lange rokade gaat de koning naar de c-lijn en de toren van de a- naar de
d-lijn.

De rokade mag niet worden uitgevoerd als:

- er al een zet met de koning gedaan is
- er al een zet met de betrokken toren is gedaan,
- er een stuk tussen de koning en de betrokken toren staat
- de koning schaak staat,
- de koning tijdens het rokeren een veld passeert dat door een vijandelijk stuk
  bestreken wordt,
- het resultaat zou zijn dat de koning na de rokade schaak komt te staan (maar
  dat spreekt vanzelf).

### En passant

En passant slaan (Fr. in het voorbijgaan, terloops) is een term uit het
schaakspel die een bepaalde manier aangeeft waarop een pion een andere pion kan
slaan. Een pion mag in de uitgangsstelling twee velden vooruit. Als het veld dat
hij daarbij passeert door een pion van de tegenstander bedreigd wordt (de pion
'eindigt' dan derhalve naast de vijandelijke pion) mag die pion hem slaan ('en
passant' = in het voorbijgaan), alsof hij slechts één veld vooruit was gezet. En
passant slaan is alleen toegestaan bij de zet direct volgend op de opmars van de
pion. Op elk moment is er hoogstens 1 en passant locatie.

### Promotie

Als een pion de overkant van het bord bereikt wordt die onmiddellijk vervangen
door een dame, paard, loper of toren. Meestal wordt er gekozen voor een dame.

## Unicode Schaakbord Notatie

We zullen borden voorstellen als een unicode raster. Een start schaakbord ziet
er als volgt uit, we gebruiken de conventie dat zwart altijd diens koning op
lijn 8 heeft staan in het begin.

```
8 ♜♞♝♛♚♝♞♜ [♛♚]
7 ♟♟♟♟♟♟♟♟
6
5
4
3
2 ♙♙♙♙♙♙♙♙
1 ♖♘♗♕♔♗♘♖ [♕♔]☚
  abcdefgh
```

Elke lijn van de notatie behalve de laatste is als volgt gestructureerd:

- Een cijfer dat het rijnummer aangeeft (8 tot 1)
- Een spatie
- Een beschrijving van de acht vakken van de rij
  - Een leeg vakje wordt genoteerd als een spatie `" "`
  - Een vak met een stuk op wordt genoteerd met het unicode symbool voor dat
    stuk (zie tabel hieronder)
- Enkel op rijen 8 en 1: extra metadata
  - Een spatie
  - Open vierkant haakje `"["`
  - Een koningin in de gepaste kleur (♛ of ♕) als er nog een rokademogelijkheid
    is langs koninginnenvleugel (lange rokade, toren op a is nog niet
    verplaatst) anders een spatie
  - Een koning in de gepaste kleur ( ♚ of ♔ ) als er nog een rokademogelijkheid
    is langs koningsvleugel (korte rokade, toren op h is nog niet verplaatst)
    anders een spatie
  - En-passantmogelijkheid: De coördinaten van het veld overgeslaan in de vorige
    beurt. Als er in de vorige zet geen veld werd overgeslagen, niets.
    Bijvoorbeeld: na de zet e2-e4 wordt "e3" aangegeven bij wit.
  - Sluit vierkant haakje `"]"`
  - Als wit aan de beurt is, en dit is rij 1 of als zwart aan de beurt is en dit
    is rij 8:
    - De aanduiding van de huidige speler (☚)

| Naam                      | Symbool | Code punt | Prolog String |
| ------------------------- | ------- | --------- | ------------- |
| witte koning              | ♔       | U+2654    | `"\u2654"`    |
| witte koningin            | ♕       | U+2655    | `"\u2655"`    |
| witte toren               | ♖       | U+2656    | `"\u2656"`    |
| witte loper               | ♗       | U+2657    | `"\u2657"`    |
| witte paard               | ♘       | U+2658    | `"\u2658"`    |
| witte pion                | ♙       | U+2659    | `"\u2659"`    |
| zwarte koning             | ♚       | U+265A    | `"\u265A"`    |
| zwarte koningin           | ♛       | U+265B    | `"\u265B"`    |
| zwarte toren              | ♜       | U+265C    | `"\u265C"`    |
| zwarte loper              | ♝       | U+265D    | `"\u265D"`    |
| zwarte paard              | ♞       | U+265E    | `"\u265E"`    |
| zwarte pion               | ♟︎      | U+265F    | `"\u265F"`    |
| aanduiding huidige speler | ☚       | U+261A    | `"\u261A"`    |

Je mag er vanuit gaan dat borden die je als input krijgt geldig zijn volgens
bovenstaand formaat en zullen niet in schaakmat stelling zijn. Het kan wel zijn
dat het bord in patstelling is, in dat geval schrijf je `DRAW` uit. Gezien de
bordnotatie niet bijhoud hoeveel zetten er geweest zijn hoef je geen rekening te
houden met de 50 zetten regel (of de 75 halve zetten regel). Ook met de "drie
keer dezelfde stelling" regel moet geen rekening worden gehouden.

# Indienen

## Bestandenstructuur

Je project moet volgende structuur hebben:

- `src/` bevat alle broncode (inclusief `main.pl`).
- `tests/` alle testcode.
- `extra/verslag.pdf` bevat de elektronische versie van je verslag. In deze map
  kun je ook eventueel extra bijlagen plaatsen.

Je directory structuur ziet er dus ongeveer zo uit:

```
|
|-- extra/
|   `-- verslag.pdf
|-- src/
|   |-- main.pl
|   `-- je broncode
`-- tests/
    `-- je testcode
```

## Compileren

De code zal bij het indienen getest worden met de opdracht
`swipl -s src/main.pl` door SubGIT met SWI Prolog versie 8.2.4. De Dockerfile en
bijhorende bronbestanden die SubGit gebruikt om je code te compileren en
minimale testen op uit te voeren vind je op
`git@subgit.ugent.be:2020-2021/LP-dockerfile`. Je kunt deze Docker ook
onmiddellijk van Dockerhub halen met volgende commando's:

```bash
docker pull beardhatcode/lp-project-2020-2021:latest
docker run -it --rm --mount type=bind,source={PAD},destination=/submission,readonly beardhatcode/lp-project-2020-2021:latest
```

Waarbij `{PAD}` vervangen moet worden met het absolute pad naar je project.

## SubGIT

Het indienen gebeurt via het [SubGit](https://subgit.ugent.be/) platform. Als je
hier nog geen account op hebt, dien je deze aan te maken.

### Repository afhalen

```bash
git clone git@subgit.ugent.be:2020-2021/LP/{studentnr} schaakcomputer
```

### Opgave als upstream instellen

Je kunt de opgave en boilerplate voor het project afhalen door de opgave
repository als upstream met volgende commando's in de `schaakcomputer` map:

```bash
git remote add upstream git@subgit.UGent.be:2020-2021/LP-assignment
git pull upstream master
```

Je kunt de laatste versie van de opgave afhalen met `git pull upstream master`.
Als je geen wijzigingen hebt aangebracht aan de `opgave` map zou die pull steeds
zonder problemen moeten verlopen.

### Feedback

Als je pusht naar SubGIT, zul je in je terminal te zien krijgen of je code
voldoet aan de minimumvereisten. In dat geval krijg je bij het pushen de melding
dat het pushen geslaagd is:

```
remote: Acceptable submission
```

Je kunt geen code pushen naar de `master` branch als die niet compileert of niet
aan de minimale IO vereisten voldoet. Je kunt steeds pushen naar alle andere
branches en daar zal je push aanvaard worden ongeacht het slagen van de testen.

### `master` branch

De `master` branch op SUBGit stelt jouw ingave voor. Je kunt voor de deadline
zoveel pushen als je wilt. Zorg ervoor dat je voor de deadline zeker je finale
versie naar de **`master`** branch hebt gepushed. (niet: `main`, wel: `master`)

### Controleren of je zeker goed hebt ingediend

Je kunt je indiening bekijken door je repository nog eens te klonen in een
andere map

```
cd eenAndereMap
git clone git@subgit.ugent.be:2020-2021/LP/{studentnr} projectLPControle
```

## Deadlines

Na **2021-06-18 om 23:23:23** kun je geen wijzigingen meer aanbrengen aan je
SubGIT repo. De code in de `src` map, de testen in de `test` map en het verslag
in `extra/verslag.pdf` zijn je finale indiening. Voor de deadline kun je zo veel
pushen naar master als je wilt, we raden je ook aan geregeld te pushen.

# Algemene richtlijnen

- Schrijf efficiënte code, maar ga niet over-optimaliseren: **geef de voorkeur
  aan elegante, goed leesbare code** Kies zinvolle namen voor predicaten en
  variabelen en voorzie voldoende commentaar.
- Deze opgave beschrijft hoe je kunt testen of jouw code aan de minimumvereisten
  van het project voldoet. Als de "docker" container niet `AANVAAARD`
  uitschrijft en stopt met exit code 0 voldoet je code niet aan de minimale
  vereisten. Projecten die niet aan de minimale vereisten voldoen kunnen geen
  punten opleveren. Het "hard coderen" van testgevallen is niet toegelaten.
- Het project wordt gequoteerd op **10** van de 20 te behalen punten voor dit
  vak. Als de helft niet wordt behaald, is je eindscore het minimum van je
  examencijfer en je score op het project.
- Projecten die ons niet (via de `master` branch op SubGIT) bereiken voor de
  deadline worden niet meer verbeterd: dit betekent het verlies van alle te
  behalen punten voor het project.
- Dit is een individueel project en dient dus door jou persoonlijk gemaakt te
  worden. **Het is ten strengste verboden code uit te wisselen**, op welke
  manier dan ook. Het overnemen van code beschouwen we als fraude (van **beide**
  betrokken partijen) en zal in overeenstemming met het examenreglement
  behandeld worden. Het overnemen of aanpassen van code gevonden op internet is
  ook **niet toegelaten** en wordt gezien als fraude.
- Vragen worden mogelijks **niet** meer beantwoord tijdens de laatste week voor
  de finale deadline.

# Vragen

Als je vragen hebt over de opgave of problemen ondervindt, dan kun je je vraag
stellen via het forum. Contacteer je ons per mail, stuur dan ook je
studentennummer (dan kunnen we gemakkelijk je code clonen) en een
"[minimal breaking example](https://stackoverflow.com/help/minimal-reproducible-example)".
Stuur geen screenshots van code.
