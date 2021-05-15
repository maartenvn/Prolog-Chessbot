# Prolog Chess Bot

Chess bot written in Prolog. This bot will take a chess board from stdin and print the best next move, determined by alpha-beta pruning, to stdout.
This chess bot was written as project for the course [Logical Programming 2020](https://studiegids.ugent.be/2020/EN/studiefiches/C003783.pdf) at [Ghent University](https://ugent.be)

## Install

Make sure to install the [SWI Prolog Suite (swipl)](https://www.swi-prolog.org/download/stable) and [GNU Make](https://www.gnu.org/software/make/) before getting started.

## Running

### All moves Mode

This mode will take a board from stdin and return all possible next moves to stdout.

```bash
cat boards/start.txt | make all-moves
```

### Next move Mode

This mode will take a board from stdin and return the next best move, determined by alpha-beta pruning, to stdout.

```bash
cat boards/start.txt | make next-move
```

## Testing

To run all available unit tests:

```bash
make test
```

## Building docs

To build the PLDocs run:

```bash
make docs
```
