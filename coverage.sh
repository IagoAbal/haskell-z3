#!/usr/bin/env bash

mkdir -p api-coverage/C
mkdir -p api-coverage/Base
mkdir -p api-coverage/Monad

cd api-coverage

# API documentation
curl -s https://z3prover.github.io/api/html/group__capi.html | grep -e "Z3_.*" | sed -e "s/\s//g" | grep -oe "Z3_\\w*</a>(" | grep -oe "Z3_\\w*" | sort | uniq > doc-api.txt
NUM_TOTAL_DOC="$(cat doc-api.txt | wc -l)"

# C API
cat ../src/Z3/Base/C.hsc | grep "foreign import ccall unsafe" | sed -e "s/foreign import ccall unsafe \"\(.*\)\"/\1/g" | sort | uniq > C-api.txt
comm -23 doc-api.txt C-api.txt > C/missing.txt
comm -13 doc-api.txt C-api.txt > C/outdated.txt

function print_coverage {
	NUM_MISSING=$1
	NUM_TOTAL=$2
	NUM_OUTDATED=$3

	COV1=$(python3 -c "print(round((1 - $NUM_MISSING/$NUM_TOTAL_DOC)*100, 2))")
	echo "  Covered: $((NUM_TOTAL_DOC-NUM_MISSING)) / $NUM_TOTAL_DOC ($COV1%)"

	if [[ ! -z $2 ]] && [[ ! -z $3 ]]; then
		COV2=$(python3 -c "print(round($NUM_OUTDATED/$NUM_TOTAL*100, 2))")
		echo "  Outdated: $NUM_OUTDATED / $NUM_TOTAL ($COV2%)"
	fi
}

echo "C API"
print_coverage $(cat C/missing.txt | wc -l) $(cat C-api.txt | wc -l) $(cat C/outdated.txt | wc -l)

echo "Base API"
expect=$(cat doc-api.txt | sed -e "s/Z3//g" | sed -r 's/(^|_)([a-z])/\U\2/g' | sed -e 's/^./\L&/')

NUM_MISSING=0
for e in $expect; do
	grep -q $e ../src/Z3/Base.hs || ((NUM_MISSING=NUM_MISSING+1))
done

print_coverage $NUM_MISSING

echo "Monad API"
NUM_MISSING=0
for e in $expect; do
	grep -q $e ../src/Z3/Monad.hs || ((NUM_MISSING=NUM_MISSING+1))
done

print_coverage $NUM_MISSING
