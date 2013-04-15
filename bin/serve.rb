#!/usr/bin/env ruby

require 'rubygems'
require 'rack'

pwd = Dir.pwd
server = Rack::Server.new(
  :app => Rack::Directory.new(pwd, Rack::File.new(pwd)),
  :Host => 'localhost', :Port => 8080)

puts "Starting server on localhost:8080"
server.start
