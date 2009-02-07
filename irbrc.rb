# -*- mode: ruby; -*-

%w{irb/completion irb/ext/save-history pp rubygems}.map(&method(:require))
begin
  require 'what_methods'
rescue LoadError; end

IRB.conf[:AUTO_INDENT] = true
IRB.conf[:SAVE_HISTORY] = 100
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb.history"

if IRB.conf[:PROMPT]
  IRB.conf[:PROMPT][:SNAZZY] = {
    :PROMPT_I => ">> ",
    :PROMPT_C => "*> ",
    :PROMPT_N => "%i> ",
    :PROMPT_S => "%l> ",
    :RETURN   => "==> %s\n"
  }
  IRB.conf[:PROMPT_MODE] = :SNAZZY
end

class Object
  def local_methods(obj = self)
    (obj.methods - (obj.class.superclass || Object).instance_methods).sort
  end
end

# Called after the irb session is initialized and Rails has
# been loaded (props: Mike Clark).
IRB.conf[:IRB_RC] = lambda do |context|
  if defined?(ActiveRecord::Base)
    begin
      name = User.column_names.include?("name")
      login = User.column_names.include?("login")
      if name || login
        instance_eval <<RUBY
def method_missing(name, *args, &block)
  super unless args.empty? && block.nil?
  instance_variable_get("@user_\#{name}") ||
    instance_variable_set("@user_\#{name}", User.find_by_#{name ? 'name' : 'login'}(name.to_s))
end
RUBY
      end
    rescue
    end

    ActiveRecord::Base.logger = Logger.new(STDOUT)
  end
end
