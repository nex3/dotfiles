#!/usr/bin/env ruby

if ARGV[0] == "in"
  exit(STDIN.isatty ? 0 : 1)
else
  exit(STDOUT.isatty ? 0 : 1)
end
