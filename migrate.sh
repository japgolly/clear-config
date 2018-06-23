#!/bin/bash
find . -name '*.scala' -exec perl -pi -e '
  s/(?<![a-zA-Z0-9])Config(\s+[a-z]|\s*[\[\.])/ConfigDef$1/g;
  s/(?<![a-zA-Z0-9])Source(\s+[a-z]|\s*[\[\.])/ConfigSource$1/g;
  s/(?<![a-zA-Z0-9])Sources(\s+[a-z]|\s*[\[\.])/ConfigSources$1/g;
  s/ConfigParser/ConfigValueParser/g;
  s/ConfigValue/Lookup/g;
  s/obfuscateInReport/secret/g;
' {} +
