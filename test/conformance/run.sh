#!/bin/sh
# Run chibi-scheme's R7RS test suite (tests/r7rs-tests.scm) against iota and
# compare the outcome with expected-failures.txt. README.org has the details.
#
#   test/conformance/run.sh /path/to/iota       or       IOTA=/path/to/iota test/conformance/run.sh
#
# Exit status:
#   0  the totals and the failing tests are exactly the ones in
#      expected-failures.txt
#   1  the totals differ, some test newly fails or newly passes, or a
#      workaround below is no longer needed
#   2  the suite could not be fetched, prepared or run to its end, or the
#      output of the run does not add up
set -eu

# The pin. The suite is fetched at this commit of ashinn/chibi-scheme and must
# have this checksum. To move the pin, change both, run again and go through
# the differences. See README.org.
commit=18e561eea6f219b4676f45b522591979c923740f
sha256=c1ef92910e70d469745a7b779658796767c1f97f7fea16a190305e396685d779
url=https://raw.githubusercontent.com/ashinn/chibi-scheme/$commit/tests/r7rs-tests.scm

# Byte order for sort, comm and cut, so the comparison does not depend on the
# caller's locale. iota reads and writes UTF-8 whatever the locale says.
LC_ALL=C
export LC_ALL

tab=$(printf '\t')

die() {
  echo "run.sh: $*" >&2
  exit 2
}

is_number() {
  case $1 in
    '' | *[!0-9]*) return 1 ;;
    *) return 0 ;;
  esac
}

iota=${1:-${IOTA:-}}
[ -n "$iota" ] || die "usage: run.sh /path/to/iota   (or set IOTA)"
command -v "$iota" >/dev/null 2>&1 || die "not an executable: $iota"

here=$(cd "$(dirname "$0")" && pwd)
cache=${IOTA_CONFORMANCE_CACHE:-$here/.cache}
mkdir -p "$cache"

suite=$cache/r7rs-tests-$commit.scm
patched=$cache/r7rs-tests-$commit.patched.scm
out=$cache/last-run.out
err=$cache/last-run.err
expected=$here/expected-failures.txt

# What the run must report. expected-failures.txt holds two totals and then
# the known failures. Read it first, so a mistake in it stops the script
# before anything is fetched or run.
bad=$(awk -F "$tab" '
  /^#/ || /^$/ { next }
  NF >= 3 { next }
  NF == 2 && ($1 == "tests" || $1 == "passed") { next }
  { print }' "$expected")
[ -z "$bad" ] || die "expected-failures.txt has a line that is neither a total nor a failure: $bad"
want_tests=$(awk -F "$tab" 'NF == 2 && $1 == "tests" { print $2 }' "$expected")
want_passed=$(awk -F "$tab" 'NF == 2 && $1 == "passed" { print $2 }' "$expected")
if ! is_number "$want_tests" || ! is_number "$want_passed"; then
  die "expected-failures.txt must have one 'tests' and one 'passed' line, each a name, a tab and a number"
fi
want_failed=$(awk -F "$tab" '!/^#/ && NF >= 3 { n++ } END { print n + 0 }' "$expected")
[ "$((want_tests - want_passed))" -eq "$want_failed" ] ||
  die "expected-failures.txt gives $want_tests tests and $want_passed passed but lists $want_failed failures"

# Fetch. The suite is third-party code under chibi-scheme's BSD licence, so it
# is downloaded on demand and never checked in.
if [ ! -f "$suite" ]; then
  echo "fetching $url"
  if ! curl -fsSL -o "$suite.part" "$url"; then
    rm -f "$suite.part"
    die "could not fetch the suite"
  fi
  mv "$suite.part" "$suite"
fi
got=$(sha256sum "$suite" | cut -d' ' -f1)
[ "$got" = "$sha256" ] ||
  die "checksum mismatch for $suite: expected $sha256, got $got. Delete the file to fetch it again."

# Workarounds. iota reads a whole file before it runs any of it, and expands
# a whole top level form before it runs any of that, so two things in the
# suite would stop the run before the first test. Both are deviations of iota
# from the report and both are reported below.
#
# 1. The reader rejects a radix prefix inside a bytevector literal, as in
#    #u8(#x41 #x42 #x43). The suite writes eight tests of utf8->string and
#    string->utf8 that way. Their five distinct bytes are rewritten in
#    decimal. The tests themselves still run.
# 2. The expander rejects a syntax-rules form whose ellipsis is also in the
#    literals list, at lines 595 to 600:
#
#      (let ()
#        (define-syntax elli-lit-1
#          (syntax-rules ... (...)
#            ((_ x)
#             '(x ...))))
#        (test '(100 ...) (elli-lit-1 100)))
#
#    The form is deleted and its one test is counted as a failure.
rewritten=$(grep -c '#u8([^)]*#x' "$suite" || true)
sed -n '596p' "$suite" | grep -q 'define-syntax elli-lit-1' ||
  die "line 596 of the suite is not the elli-lit-1 definition. Revisit workaround 2."
sed -n '600p' "$suite" | grep -q '(elli-lit-1 100)))' ||
  die "line 600 of the suite is not the end of the elli-lit-1 form. Revisit workaround 2."
sed -e '/#u8(/{s/#x41/65/g;s/#x42/66/g;s/#x43/67/g;s/#xCE/206/g;s/#xBB/187/g;}' \
    -e '595,600d' "$suite" > "$patched"
if grep -q '#u8([^)]*#' "$patched"; then
  die "a bytevector literal still has a prefixed byte after workaround 1"
fi
removed="4.3 Macros${tab}(elli-lit-1 100)${tab}#1"
removed_count=1

# Run. The suite imports (chibi test), which resolves to chibi/test.iota here.
status=0
IOTA_LIBRARY_PATH=$here "$iota" "$patched" > "$out" 2> "$err" || status=$?

# Trust the output only if it adds up. iota must have exited normally. Every
# line must be one of the three kinds chibi/test.iota prints. There must be
# one TOTAL line, at the very end, and its figures must agree with the FAIL
# and GROUP lines before it. Otherwise something was lost on the way, and
# comparing what is left with the expected failures would say nothing.
if ! grep -q "^TOTAL$tab" "$out"; then
  echo "run.sh: the suite did not run to its end (iota exited with $status):" >&2
  cat "$err" >&2
  exit 2
fi
if [ "$status" -ne 0 ]; then
  echo "run.sh: iota exited with $status although the suite printed its totals:" >&2
  cat "$err" >&2
  exit 2
fi
stray=$(grep -Ev "^(FAIL|GROUP|TOTAL)$tab" "$out" || true)
[ -z "$stray" ] || die "the run printed lines that are not FAIL, GROUP or TOTAL: $stray"
[ "$(grep -c "^TOTAL$tab" "$out")" -eq 1 ] ||
  die "the run printed more than one TOTAL line"
tail -n 1 "$out" | grep -q "^TOTAL$tab" ||
  die "the run printed more output after its TOTAL line"

run_tests=$(awk -F "$tab" '$1 == "TOTAL" { print $2 }' "$out")
run_passed=$(awk -F "$tab" '$1 == "TOTAL" { print $3 }' "$out")
run_failed=$(awk -F "$tab" '$1 == "TOTAL" { print $4 }' "$out")
if ! is_number "$run_tests" || ! is_number "$run_passed" || ! is_number "$run_failed"; then
  die "the TOTAL line of the run is malformed"
fi
fail_lines=$(grep -c "^FAIL$tab" "$out" || true)
group_passed=$(awk -F "$tab" '$1 == "GROUP" { n += $3 } END { print n + 0 }' "$out")
group_failed=$(awk -F "$tab" '$1 == "GROUP" { n += $4 } END { print n + 0 }' "$out")
[ "$((run_passed + run_failed))" -eq "$run_tests" ] ||
  die "TOTAL says $run_tests tests but $run_passed passed and $run_failed failed"
[ "$fail_lines" -eq "$run_failed" ] ||
  die "TOTAL says $run_failed tests failed but there are $fail_lines FAIL lines"
if [ "$group_passed" -ne "$run_passed" ] || [ "$group_failed" -ne "$run_failed" ]; then
  die "the GROUP lines add up to $group_passed passed and $group_failed failed, TOTAL says $run_passed and $run_failed"
fi

# The test deleted by workaround 2 is part of the suite and iota fails it.
tests=$((run_tests + removed_count))
passed=$run_passed
failed=$((run_failed + removed_count))

echo "chibi-scheme tests/r7rs-tests.scm at $commit"
echo
awk -F "$tab" '$1 == "GROUP" { printf "  %-34s %4d passed %3d failed\n", $2, $3, $4 }' "$out"
echo
awk -F "$tab" '$1 == "FAIL" { printf "FAIL [%s] %s %s: %s\n", $2, $3, $4, $5 }' "$out"
printf 'FAIL [%s] %s %s: %s\n' "4.3 Macros" "(elli-lit-1 100)" "#1" \
  "not run, iota rejects the form at expansion time (workaround 2)"
echo
echo "$tests tests: $passed passed, $failed failed"
echo "  $removed_count of the failures could not be run and was removed (workaround 2)"
echo "  $rewritten of the passes ran with a rewritten bytevector literal (workaround 1)"

# Compare the totals. The list of failures alone would not show a change, to
# the suite or to chibi/test.iota, that makes tests disappear.
totals=
if [ "$tests" -ne "$want_tests" ]; then
  totals="${totals}  tests: expected $want_tests, got $tests
"
fi
if [ "$passed" -ne "$want_passed" ]; then
  totals="${totals}  passed: expected $want_passed, got $passed
"
fi

# Compare the failures. A failure is identified by its group, its name and
# its number among the tests of that name in the group.
{
  awk -F "$tab" -v OFS="$tab" '$1 == "FAIL" { print $2, $3, $4 }' "$out"
  printf '%s\n' "$removed"
} | sort > "$cache/actual.keys"
grep -v '^#' "$expected" | awk -F "$tab" 'NF >= 3' | cut -f1-3 | sort > "$cache/expected.keys"

new_failures=$(comm -23 "$cache/actual.keys" "$cache/expected.keys")
new_passes=$(comm -13 "$cache/actual.keys" "$cache/expected.keys")

# A workaround hides the thing it works around, so check separately whether
# iota still needs each one.
stale=
if "$iota" eval '(bytevector-u8-ref #u8(#x41) 0)' >/dev/null 2>&1; then
  stale="${stale}  1: the reader now accepts #u8(#x41)
"
fi
if "$iota" eval "(define-syntax m (syntax-rules ... (...) ((_ x) '(x ...)))) (m 100)" >/dev/null 2>&1; then
  stale="${stale}  2: the expander now accepts an ellipsis that is also a literal
"
fi

if [ -z "$totals" ] && [ -z "$new_failures" ] && [ -z "$new_passes" ] && [ -z "$stale" ]; then
  echo "The totals and the failures are exactly the ones in expected-failures.txt."
  exit 0
fi
echo
if [ -n "$totals" ]; then
  echo "Totals that differ from expected-failures.txt:"
  printf '%s' "$totals"
fi
if [ -n "$stale" ]; then
  echo "Workarounds in run.sh that iota no longer needs:"
  printf '%s' "$stale"
fi
if [ -n "$new_failures" ]; then
  echo "Failing, but not in expected-failures.txt:"
  printf '%s\n' "$new_failures" | sed 's/^/  /'
fi
if [ -n "$new_passes" ]; then
  echo "In expected-failures.txt, but no longer failing:"
  printf '%s\n' "$new_passes" | sed 's/^/  /'
fi
exit 1
