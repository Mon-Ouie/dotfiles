# -*- coding: utf-8 -*-
require 'rubygems'

libs = %w(bond what_methods ap pp socket open-uri)

init = {
  :wirble => lambda { Wirble.init },
  :bond => lambda { Bond.start },
  :ap => lambda {
    # awesome_print doesn't want me to use color in inf-ruby :(
    class String
      %w[gray red green yellow blue purple cyan white].each_with_index do |color, i|
        define_method color          do "\033[1;#{30+i}m#{self}\033[0m" end
        define_method :"#{color}ish" do "\033[0;#{30+i}m#{self}\033[0m" end
      end
    end

    class AwesomePrint
      alias :old_mcd! :merge_custom_defaults!

      def merge_custom_defaults!
        colors = {
          :module     => :yellowish,
          :string     => :cyanish,
          :symbol     => :cyan,
          :array      => :pale,
          :hash       => :white,
          :bignum     => :blueish,
          :bigdecimal => :blueish,
          :complex    => :blueish,
          :rational   => :blueish,
        }

        @options[:color].merge!(colors)
        @options[:indent] = 2

        old_mcd!
      end

      def awesome_rational(n)
        awesome_self n, :with => " ≈ #{n.to_f}"
      end
    end

    IRB::Irb.class_eval do
      alias :non_color_output_value :output_value

      def output_value
        if @context.inspect?
          ap @context.last_value
        else
          non_color_output_value
        end
      end
    end
  }
}

libs.each do |ext|
  begin
    require(ext)
    if proc = init[ext.to_sym]
      proc.call
    end
  rescue LoadError
    $stderr.puts "couldn't load #{ext}"
  end
end

ANSI = {}
ANSI[:RESET]     = "\e[0m"
ANSI[:BOLD]      = "\e[1m"
ANSI[:UNDERLINE] = "\e[4m"
ANSI[:LGRAY]     = "\e[0;37m"
ANSI[:GRAY]      = "\e[1;30m"
ANSI[:RED]       = "\e[31m"
ANSI[:GREEN]     = "\e[32m"
ANSI[:YELLOW]    = "\e[33m"
ANSI[:BLUE]      = "\e[34m"
ANSI[:MAGENTA]   = "\e[35m"
ANSI[:CYAN]      = "\e[36m"
ANSI[:WHITE]     = "\e[37m"

def method_info(method)
  args = if method.respond_to?(:parameters) && (arg_ary = method.parameters)
           arg_ary.map!.each_with_index do |(type, name), index|
             name ||= "arg#{index + 1}"

             case type
             when :req   then "#{name}"
             when :opt   then "#{name} = ?"
             when :rest  then "*#{name}"
             when :block then "&#{name}"
             else name
             end
           end

           '(' + arg_ary.join(', ') + ')'
         else
           if method.arity == 0
             "()"
           elsif method.arity > 0
             n = method.arity
             '(' + (1..n).map { |i| "arg#{i}" }.join(", ") + ')'
           elsif method.arity < 0
             n = -method.arity
             '(' + (1..n).map { |i| "arg#{i}" }.join(", ") + ')'
           end
         end

  klass = if method.respond_to? :owner
            method.owner.name
          elsif method.inspect =~ /Method: (.*?)#/
            $1
          end

  location = if method.respond_to? :source_location
               file, line = method.source_location
               "#{file}:#{line}" if file && line
             end

  [method.name.to_s, args, klass.to_s, location]
end

def print_method_list(methods, *options, &block)
  methods -= Object.instance_methods unless options.include? :more

  filter  = options.select { |opt| opt.is_a? Regexp }.first
  methods = methods.grep(filter) if filter

  use_location = options.include? :loc

  data = methods.sort.map do |name|
    method_info block.call(name)
  end

  max_name  = data.map { |item| item[0].size }.max
  max_args  = data.map { |item| item[1].size }.max
  max_klass = data.map { |item| item[2].size }.max

  data.each do |(name, args, klass, location)|
    str =  " #{ANSI[:YELLOW]}#{name.rjust(max_name)}#{ANSI[:RESET]}"
    str << "#{ANSI[:BLUE]}#{args.ljust(max_args)}#{ANSI[:RESET]}"
    str << " #{ANSI[:GRAY]}#{klass.ljust(max_klass)}#{ANSI[:RESET]}"
    str << " (#{location})" if use_location && location

    puts str
  end

  data.size
end

def pm(obj, *options)
  print_method_list(obj.methods(!options.include?(:less)), *options) do |name|
    obj.method(name)
  end
end

def pcm(klass, *options)
  print_method_list(klass.instance_methods(!options.include?(:less)), *options) do |name|
    klass.instance_method(name)
  end
end

def ivars(obj = self)
  hash = {}
  obj.instance_variables.each do |name|
    hash[name.to_s] = obj.instance_variable_get(name)
  end

  hash
end

def gvars
  hash = {}
  global_variables.each do |name|
    hash[name.to_s] = eval(name.to_s)
  end

  hash
end

def cvars(klass)
  hash = {}
  klass.class_variables.each do |name|
    hash[name.to_s] = klass.class_variable_get(name)
  end

  hash
end

def consts(klass = Object)
  hash = {}
  klass.constants.each { |name| hash[name.to_s] = klass.const_get(name) }
  hash
end

def ri(symbol, command = 'ri')
  symbol = if symbol.is_a? Method
             if symbol.receiver.is_a?(Module)
               "#{symbol.receiver}.#{symbol.name}"
             else
               "#{symbol.owner}##{symbol.name}"
             end
           elsif symbol.is_a? UnboundMethod
             "#{symbol.owner}##{symbol.name}"
           else
             symbol.to_s
           end

  system(command, symbol)
end

def yri(symbol)
  ri(symbol, 'yri')
end

def edit(file, line = 0)
  if file.respond_to?(:source_location) && loc = file.source_location
    edit loc[0], loc[1] + line
  else
    system "#{ENV["EDITOR"]} +#{line} '#{file}'"

    if File.exist? file
      eval(File.read(file), TOPLEVEL_BINDING)
    end
  end
end

IRB.conf[:PROMPT_MODE] = :SIMPLE

$: << '.'

if File.exist? "lib"
  $:.unshift "./lib"
  $:.unshift "./ext" if File.exist? "ext"

  begin
    require File.basename(Dir.pwd)
  rescue LoadError
  end
end
