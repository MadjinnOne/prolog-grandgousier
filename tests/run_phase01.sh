#!/usr/bin/env bash

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
swipl -s "${REPO_ROOT}/tests/phase01_test.pl" -g run_tests -t halt
