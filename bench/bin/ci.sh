#!/usr/bin/env bash
set -e

if [ "$#" -ne 3 ]; then
  echo "Expected 3 arguments: MODE PR MERGED"
  echo "  MODE: 'notNightly' or 'nightly' or 'all'"
  echo "  PR: the PR associated with the current commit"
  echo "  MERGED: 'true' if the PR is merged, 'false' otherwise"
  exit 1
fi

MODE=$1
PR=$2
MERGED=$3
COMMIT_DATE=$(git --no-pager show --quiet --format='%cI')
COMMIT=$(git rev-parse --short=16 HEAD)
BENCH_DATE=$(date -u +'%Y-%m-%dT%H:%M:%SZ')

case "$MODE" in
  "notNightly")
    # Select benchmarks that do not contain `Nightly` or `Bootstrapped` in their name.
    JMH_FILTERS="-e Bootstrapped -e Nightly"
    # Select benchmarks that contain `Bootstrapped` in their name, but not `Nightly`.
    JMH_BOOTSTRAP_FILTERS="-e Nightly Bootstrapped"
    ;;
  "nightly")
    JMH_FILTERS="-e Bootstrapped Nightly"
    JMH_BOOTSTRAP_FILTERS="BootstrappedNightly"
    ;;
  "all")
    JMH_FILTERS="-e Bootstrapped"
    JMH_BOOTSTRAP_FILTERS="Bootstrapped"
    ;;
  *)
    echo "Expected first argument to be either 'notNightly' or 'nightly' or 'all'."
    exit 1
    ;;
esac

if [[ -z "${DATA_CSV_PATH}" ]]; then
  echo " DATA_CSV_PATH must set to the benchmarks CSV data file."
  exit 1
fi

if [[ -z "${WEBSITE_DEST}" ]]; then
  echo "WEBSITE_DEST must be set the destination to which the website should be deployed."
  exit 1
fi

# `-foe true` means "fail on error".
# `-gc true` launches the garbage collector between each iterations, which significantly reduces noise.
JMH_ARGS="-foe true -gc true -wi 0 -i 1"

JMH_OUTPUT_PATH="jmh-output.txt"
VIZUALIZER_PATH="bench/vizualizer"
VIZUALIZER_DATA_PATH="$VIZUALIZER_PATH/data"

echo "::group::Run benchmarks"
set -x
# sbt "scala3-bench / Jmh / run $JMH_ARGS $JMH_FILTERS; scala3-bench-bootstrapped / Jmh / run $JMH_ARGS $JMH_BOOTSTRAP_FILTERS" | tee $JMH_OUTPUT_PATH
set +x
echo "::endgroup::"

echo "::group::Import results"
scala bench/scripts --main-class importResults -- $PR $MERGED $COMMIT_DATE $COMMIT $BENCH_DATE $JMH_OUTPUT_PATH $DATA_CSV_PATH
echo "::endgroup::"

echo "::group::Generate vizualizer data"
scala bench/scripts --main-class makeVizualizerData -- $DATA_CSV_PATH $VIZUALIZER_DATA_PATH
echo "::endgroup::"

echo "::group::Deploy vizualizer"
rsync -av $VIZUALIZER_PATH/ $WEBSITE_DEST
echo "::endgroup::"
