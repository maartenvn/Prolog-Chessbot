# All possible next moves
all-moves:
	swipl -f none -t halt -g main -q src/main.pl TEST

# Best next move
next-move:
	swipl -f none -t halt -g main -q src/main.pl

# Run tests
test:
	swipl -g run_tests -t halt tests/*.pl

# Build docs
docs:
	echo "TODO"