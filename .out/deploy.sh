#!/usr/bin/env bash

set -e # exit with nonzero exit code if anything fails

git init
git add .

git commit --author "Travis CI <>" --message "Deploy to GitHub Pages"

GITHUB_URL=https://$GITHUB_TOKEN@github.com/chris-martin/project-euler.git

# Redirect any output to /dev/null to avoid exposing the GitHub access token.
git push --force --quiet $GITHUB_URL master:gh-pages > /dev/null 2>&1
