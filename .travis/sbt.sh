#!/usr/bin/env bash
cd scala
export JAVA_HOME=/usr/lib/jvm/java-8-oracle
./sbt "$@"
