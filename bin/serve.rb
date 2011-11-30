#!/usr/bin/env ruby

require 'rubygems'
require 'rack'

puts "Starting server on localhost:8080"
Rack::Server.new(:app => Rack::File.new(Dir.pwd),
  :Host => 'localhost', :Port => 8080).start
