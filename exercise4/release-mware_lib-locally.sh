#!/usr/bin/env bash
set -eu
git stash
git pull
git checkout tags/mware_lib-0.2.0
cd ./middleware/
./gradlew clean install
git checkout master
git stash pop
