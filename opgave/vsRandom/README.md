# Chess Tests

Met dit nodejs project kan je jouw schaakcomputer testen, je zult afhankelijk
van je systeem eerst nog de volgende stappen moeten ondernemen.

- NodeJS installeren
- `node TestEngine.js`

Eens je deze stappen hebt ondernomen kan je jouw engine testen als volgt:

```
$ node TestEngine.js
Random sets the board up for you to start as white



🔀 RANDOM:
8 ♜♞♝♛♚♝♞♜ [♛♚]
7 ♟♟♟♟♟♟♟♟
6
5
4
3
2 ♙♙♙♙♙♙♙♙
1 ♖♘♗♕♔♗♘♖ [♕♔]☚
  abcdefgh



🤖👌 PL OK  (10.00ms, avg: 10.00ms)
8 ♜♞♝♛♚♝♞♜ [♛♚]☚
7 ♟♟♟♟♟♟♟♟
6
5
4
3 ♙
2  ♙♙♙♙♙♙♙
1 ♖♘♗♕♔♗♘♖ [♕♔]
  abcdefgh



🔀 RANDOM:
8 ♜♞♝♛♚♝♞♜ [♛♚b6]
7 ♟ ♟♟♟♟♟♟
6
5  ♟
4
3 ♙
2  ♙♙♙♙♙♙♙
1 ♖♘♗♕♔♗♘♖ [♕♔]☚
  abcdefgh



🤖❌ PL FAIL
8 ♜♞♝♛♚♝♞♜ [♛♚]
7 ♟♟♟♟♟♟♟♟
6
5
4
3
2 ♙♙♙♙♙♙♙♙
1 ♖♘♗♕♔♗♘♖ [♕♔]☚
  abcdefgh
Not a valid move
Valid move example:
8 ♜♞♝♛♚♝♞♜ [♛♚]☚
7 ♟ ♟♟♟♟♟♟
6
5  ♟
4 ♙
3
2  ♙♙♙♙♙♙♙
1 ♖♘♗♕♔♗♘♖ [♕♔]
  abcdefgh

ALL valid moves (shortend FEN notation):
rnbqkbnr/p1pppppp/8/1p6/P7/8/1PPPPPPP/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/8/PP6/2PPPPPP/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/1P6/P7/2PPPPPP/RNBQKBNR b KQkq b3
rnbqkbnr/p1pppppp/8/1p6/8/P1P5/1P1PPPPP/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/2P5/P7/1P1PPPPP/RNBQKBNR b KQkq c3
rnbqkbnr/p1pppppp/8/1p6/8/P2P4/1PP1PPPP/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/3P4/P7/1PP1PPPP/RNBQKBNR b KQkq d3
rnbqkbnr/p1pppppp/8/1p6/8/P3P3/1PPP1PPP/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/4P3/P7/1PPP1PPP/RNBQKBNR b KQkq e3
rnbqkbnr/p1pppppp/8/1p6/8/P4P2/1PPPP1PP/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/5P2/P7/1PPPP1PP/RNBQKBNR b KQkq f3
rnbqkbnr/p1pppppp/8/1p6/8/P5P1/1PPPPP1P/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/6P1/P7/1PPPPP1P/RNBQKBNR b KQkq g3
rnbqkbnr/p1pppppp/8/1p6/8/P6P/1PPPPPP1/RNBQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/7P/P7/1PPPPPP1/RNBQKBNR b KQkq h3
rnbqkbnr/p1pppppp/8/1p6/8/P7/RPPPPPPP/1NBQKBNR b Kkq -
rnbqkbnr/p1pppppp/8/1p6/8/P1N5/1PPPPPPP/R1BQKBNR b KQkq -
rnbqkbnr/p1pppppp/8/1p6/8/P4N2/1PPPPPPP/RNBQKB1R b KQkq -
rnbqkbnr/p1pppppp/8/1p6/8/P6N/1PPPPPPP/RNBQKB1R b KQkq -
stderr
undefined
SEED                  :4105
```

## NodeJS installeren

Om gebruik te maken van de testcode moet je eerst nodejs installeren zie:
https://nodejs.org/en/

## Configuratie SWIPL

De testcode gaat er vanuit dat `swipl` in het `PATH` zit (op niet windows
machines). Op windows machines gaat het er vanuit dat het te vinden in
`C:\Program Files\swipl\bin\swipl`. Als dat niet het geval is kun je de lijn die
`swipl` definieert aanpassen:

```javascript
var swipl = "Q:Mijn alternatief pad naar swipl";
```

## Configuratie programma

De testcode gaat ervanuit dat je een file met de naam `main.pl` in de folder
prolog-chess-tests hebt staan. Dit kan je aanpassen bovenaan in de file
TestEngine.js.

```javascript
var project = "../../main.pl";
```

Vergeet niet dat volgens de opgave je programma te starten moet zijn vanuit
`main.pl`. Je kunt hier relative padnaam zoals `../../main.pl` opgeven. hier.

## Prolog correct afsluiten

Nadat je prolog code een nieuw bord heeft geprint moet je prolog afsluiten met
"halt" anders zal de test zeggen dat je hebt afgesloten met een ongeldige
exitcode.

```prolog
  halt(0).
```

## Seed

De testcode neemt simpelweg een random geldig stap. Indien je een fout ontdekt
wil je natuurlijk dezelfde stappen opnieuw kunnen testen. Om dit makkelijker te
maken maakt de code gebruik van een pseudo random generator waarvan je de seed
kan instellen. Door de seed in te stellen kan je ervoor zorgen dat de computer
exact dezelfde stappen zal nemen.

Indien je de code opnieuw wilt starten met een bepaalde seed, kun je die
meegeven als parameter, dus bijvoorbeeld `node TestEngine.js 1337`.

## De emoji ene icoontjes zien er niet uit

Gebruik de WSL-terminal of gelijk welke terminal die emoji en unicode
schaakstukken ondersteunt.

# Bijdragen

Indien je fouten vind in deze testcode kun je ze oplossen en de wijzigingen als
patch doorsturen naar Robbert.GurdeepSingh@UGent.be. Een patch formatteren om
toe te voegen doe je met `git format-patch HEAD^`.
