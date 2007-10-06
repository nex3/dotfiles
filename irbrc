# -*- mode: ruby; -*-

%w{irb/completion irb/ext/save-history pp rubygems}.map(&method(:require))
begin
  require 'what_methods'
rescue LoadError; end

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb.history"

IRB.conf[:PROMPT][:COLORFUL] = {
  :PROMPT_I => "\e[1;31m>>\e[0m ",
  :PROMPT_C => "\e[0;31m>>\e[0m",
  :PROMPT_N => "\e[1;34m>%i\e[0m ",
  :PROMPT_S => "\e[1;32m>%l\e[0m ",
  :RETURN   => "\e[1;29m==> %s\e[0m\n"
}
IRB.conf[:PROMPT][:PLAIN] = {
  :PROMPT_I => ">> ",
  :PROMPT_C => ">%n ",
  :PROMPT_N => ">%n ",
  :PROMPT_S => ">%l ",
  :RETURN   => "==> %s\n"
}
IRB.conf[:PROMPT_MODE] = ENV['TERM'] == 'dumb' ? :PLAIN : :COLORFUL

class Object
  def local_methods(obj = self)
    (obj.methods - (obj.class.superclass || Object).instance_methods).sort
  end
end
