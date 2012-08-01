#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

require 'colorful_inspect'
require 'term/ansicolor'

class String
  include Term::ANSIColor
end

Pry.print = proc do |output, value|
  begin
    output.print "=> "
    PP.pp(value, output)
  rescue Exception => e
    output.puts "=> (unknown: #{e})"
  end
end

prompt = Proc.new do |target_self, nest_level, pry|
  if pry
    "pry(#{Pry.view_clip(target_self)}):#{pry.input_array.size}> "
  else
    "pry(#{Pry.view_clip(target_self)})> "
  end
end

Pry.prompt = [prompt, prompt]

Pry.exception_handler = proc do |output, ex|
  output.puts "#{ex.class.name.red.bold}: #{ex.message.italic}"
  output.puts "  from #{ex.backtrace.first}"
end

Pry.pager = false

Pry.config.auto_indent    = false
Pry.config.correct_indent = false

Pry.config.color = true

Pry.config.theme = "twilight"
# Other nice themes: pry-cold, zenburn, railscasts

begin
  require 'coolline'

  Pry.config.input = Coolline.new do |cool|
    cool.word_boundaries = [" ", "\t", ",", ";", ".", '"', "'", "`", "<", ">",
                            "=", ";", "|", "{", "}", "(", ")", "-"]

    cool.history_file = Coolline::NullFile

    cool.transform_proc = proc do
      CodeRay.scan(cool.line, :ruby).term
    end
  end
rescue LoadError
end if ENV["TERM"] != "dumb"

# Custom commands

require 'slop'

module LsHelpers
  def pretty(obj)
    obj.pretty_inspect.chomp
  end

  def variables(scope, reg, verbose)
    var_array = target.eval("#{scope}_variables").grep(reg)

    if verbose
      var_hash = {}

      var_array.each do |name|
        var_hash[name.to_sym] = target.eval(name.to_s)
      end

      var_hash
    else
      var_array
    end
  end

  def constant_list(reg, verbose)
    const_array = target.eval("constants").grep(reg)

    if verbose
      const_hash = {}

      const_array.each do |name|
        const_hash[name.to_sym] = target.eval("self").const_get(name)
      end

      const_hash
    else
      const_array
    end
  end

  def method_info(method)
    args = ''

    if method.respond_to?(:parameters) && (arg_ary = method.parameters)
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

      args = '(' + arg_ary.join(', ') + ')'
    elsif method.arity == 0
      args = "()"
    elsif method.arity > 0
      n = method.arity
      args = '(' + (1..n).map { |i| "arg#{i}" }.join(", ") + ')'
    elsif method.arity < 0
      n = -method.arity
      args = '(' + (1..n).map { |i| "arg#{i}" }.join(", ") + ')'
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

  def print_method_list(output, methods, regexp, more, less, verbose, &block)
    methods -= Object.instance_methods unless more || less

    methods = methods.grep(regexp)

    data = methods.sort.map do |name|
      method_info(yield name)
    end

    max_name  = data.map { |item| item[0].size }.max
    max_args  = data.map { |item| item[1].size }.max
    max_klass = data.map { |item| item[2].size }.max

    data.each do |(name, args, klass, location)|
      str =  " #{yellow(name.rjust(max_name))}"
      str << args.ljust(max_args).blue
      str << " #{gray(klass.ljust(max_klass))}"
      str << " (#{location})" if verbose && location

      output.puts str
    end
  end

  def italic(string)
    "\033[#{3}m#{string}\033[0m"
  end

  def yellow(string)
    "\033[1;#{33}m#{string}\033[0m"
  end

  def gray(string)
    "\033[1;#{37}m#{string}\033[0m"
  end
end

module YriHelpers
  def wrap_text(text, columns = 80)
    text = text.dup
    res = []

    while text.length > columns
      if text[0, columns] =~ /^(.+\s)(\S+)$/
        res << $1
        text = $2 + text[columns..-1]
      else
        res << text[0, columns]
        text[0...columns] = ''
      end
    end

    res << text
    res
  end

  def signature_for(info)
    sig = "#{info.name.to_s.cyan}(" + info.parameters.map { |(param, default)|
      if default
        "#{param} = #{default}"
      else
        param
      end
    }.join(", ") + ")"

    if yield_tag = info.tag("yield")
      args = yield_tag.types ? yield_tag.types.join(", ") : ""
      args = "|#{args}| " unless args.empty?

      sig << " { #{args}... }"
    end

    type = (tag = info.tag("return")) ? tag.type : "Object"
    sig << " # => #{type.yellow}"
  end

  def format_parameter(param)
    types = if param.types
              param.types.map { |o| o.yellow }.join(', ')
            else
              "Object"
            end

    default = if param.respond_to? :defaults and param.defaults
                " (default: #{param.defaults.join(", ")})"
              end

    text = (param.text || "").gsub("\n", "\n" + " " * 6)
    "  — (#{types}) #{param.name.bold}#{default} #{text}"
  end

  def document_info(info, output)
    doc = info.docstring.split("\n")
    doc.each do |line|
      if line[0, 2] == " " * 2
        output.puts CodeRay.scan(line, :ruby).term
      else
        output.puts line
      end
    end

    if deprecated = info.tag("deprecated")
      output.puts
      output.puts "#{'DEPRECATED:'.red.bold} #{deprecated.text}"
    end

    if note = info.tag("note")
      output.puts
      output.puts "#{'NOTE:'.red.bold} #{note.text}"
    end

    if abstract = info.tag("abstract")
      output.puts
      output.puts "#{'Abstract:'.bold} #{abstract.text}"
    end

    unless info.tags("param").empty?
      output.puts
      output.puts "Parameters: ".italic
      info.tags("param").each do |param|
        output.puts format_parameter(param)
      end
    end

    unless info.tags("option").empty?
      info.tags("option").group_by(&:name).each do |name, opts|
        output.puts
        output.puts "Options for #{name.bold}: ".italic

        opts.each do |opt|
          output.puts format_parameter(opt.pair)
        end
      end
    end

    if yield_tag = info.tag("yield")
      output.puts
      output.print "#{'Yields:'.bold.italic} "
      output.puts yield_tag.text.to_s.gsub("\n", "\n  ")

      unless info.tags("yieldparam").empty?
        output.puts
        output.puts "Block arguments: ".bold.italic

        info.tags("yieldparam").each do |param|
          output.puts format_parameter(param)
        end
      end

      if ret = info.tag("yieldreturn")
        output.print "Block returns: ".bold.italic
        output.print "(#{(ret.types || %w[Object]).join(', ')}) "
        output.puts  ret.text.gsub("\n", "\n  ")
      end
    end

    unless info.tags("raise").empty?
      output.puts
      output.puts "Exceptions: ".bold

      info.tags("raise").each do |tag|
        output.print "  — #{(tag.types || %w[Object]).join(', ')}: ".italic
        output.puts tag.text
      end
    end

    if ret = info.tag("return")
      output.print "Returns: ".bold.italic
      output.print "(#{(ret.types || %w[Object]).join(', ')}) "
      output.puts  ret.text.to_s.gsub("\n", "\n  ")
    end

    unless info.tags("example").empty?
      info.tags("example").each do |ex|
        output.puts
        output.puts "Example: #{ex.name.bold}:".italic

        code = "  " + CodeRay.scan(ex.text, :ruby).term.gsub("\n", "\n  ")
        output.puts code
      end
    end

    unless info.tags("see").empty?
      output.puts
      output.puts "See also: ".bold

      info.tags("see").each do |tag|
        output.puts " — #{tag.text}"
      end
    end

    if author = info.tag("author")
      output.puts
      output.puts "#{'Author:'.bold} #{author.text}"
    end
  end
end
require "pry-doc"
require "yard"

Commands = Pry::CommandSet.new Pry::Commands do
  helpers do
    include YriHelpers
    include LsHelpers
  end

  # Dependency check broken?
  # alias_command "gist", "gist-method"

  # Confusing me
  alias_command "exit", "exit-all"

  command "req" do |*args|
    args.each { |arg| require arg }
  end

  command "ls", "List a lot of  stuff" do |*args|
    show_help = args.empty?

    opts = Slop.parse! args, :multiple_switches => true do
      on :v, :verbose, "Show value of variables and constants and locations of methods"
      on :L, :less, "Only show methods set by the receiver"
      on :a, :more, "Show all of the methods, including those defined in Object"
      on :f, :filter, "Regular expression to filter methods and variables",
        :argument => true, :default => ""

      on :l, :locals, "Show local variables"
      on :g, :globals, "Show global variables"
      on :i, 'instance-variables', "Show instance variables"
      on :k, 'class-variables', "Show class variables"

      on :c, :constants, "Show constants"

      on :m, :methods, "Show methods"
      on :M, 'instance-methods', "Show instance methods"

      on :h, :help, 'Print this help message', :tail => true
    end

    if show_help || opts.help?
      puts opts.help
      next
    end

    opts = opts.to_hash

    regexp = Regexp.new(opts[:filter], 'i')

    unless args.empty?
      self.target = Pry.binding_for(target.eval(args.join(' ')))
    end

    if opts[:locals]
      output.print italic("Local variables: ")
      output.puts pretty(variables(:local, regexp, opts[:verbose]))
      output.puts
    end

    if opts[:globals]
      output.print italic("Global variables: ")
      output.puts pretty(variables(:global, regexp, opts[:verbose]))
      output.puts
    end

    if opts[:"instance-variables"]
      output.print italic("Instance variables: ")
      output.puts pretty(variables(:instance, regexp, opts[:verbose]))
      output.puts
    end

    if opts[:"class-variables"]
      output.print italic("Class variables: ")

      if Module === target.eval('self')
        output.puts pretty(variables(:class, regexp, opts[:verbose]))
      else
        output.puts "(not a module)"
      end

      output.puts
    end

    if opts[:constants]
      output.print italic("Constant: ")

      if Module === target.eval('self')
        output.puts pretty(constant_list(regexp, opts[:verbose]))
      else
        output.puts "(not a module)"
      end

      output.puts
    end

    if opts[:"instance-methods"]
      output.print italic("Instance methods: ")

      if Module === target.eval('self')
        obj = target.eval("self")

        output.puts
        print_method_list(output, obj.instance_methods(!opts[:less]),
                          regexp, opts[:more], opts[:less],
                          opts[:verbose]) do |name|
          obj.instance_method(name)
        end
      else
        output.puts "(not a module)"
      end

      output.puts
    end

    if opts[:methods]
      output.puts italic("Methods: ")

      obj = target.eval("self")

      print_method_list(output, obj.methods(!opts[:less]),
                        regexp, opts[:more], opts[:less],
                        opts[:verbose]) do |name|
        obj.method(name)
      end

      output.puts
    end
  end

  command "yri", "Retrieve documentation from YARD" do |*args|
    opts = Slop.parse! args do
      on :m, "Use methods"
      on :M, "Use instance methods"
      on :s, :source, "Show source code"
    end

    info = YARD::Registry.at(args.shift)

    output.puts "-" * 80
    if (overloads = info.tags("overload")).size > 0
      overloads.each do |overload|
        output.puts wrap_text(signature_for(overload))
      end
    else
      output.puts wrap_text(signature_for(info))
    end

    output.puts "-" * 80

    document_info(info, output)

    info.tags("overload").each do |overload|
      output.puts
      output.puts "-" * 80
      output.puts "#{"Overload:".bold.italic} #{signature_for(overload)}"
      output.puts "-" * 80

      document_info(overload, output)
    end

    if opts.source?
      output.puts
      output.puts "Source code: ".bold.italic

      source = CodeRay.scan(info.source.to_s, info.source_type).term
      output.puts "  " + source.gsub("\n", "\n  ")
    end
  end

  alias_command "ri", "yri"

  alias_command "?", "show-input"
end

Pry.commands = Commands

# Apeiros' benchmarking methods

# tiny bench method
def bench(n=100, runs=10)
  n = n.to_i
  t = []
  runs.times do
    a = Time.now
    for _ in 1..n
      yield
    end
    t << (Time.now-a)*1000/n
  end
  mean = t.inject { |a,b| a+b }.quo(t.size)
  stddev = t.map { |a| (a-mean)**2 }.inject { |a,b| a+b }.quo(t.size)**0.5
  [mean, stddev]
end

# tiny bench method with nice printing
def pbench(n=1, runs=5, &b)
  m, s = *bench(n,runs,&b)
  p = (100.0*s)/m
  printf "ø %fms (%.1f%%)\n", m, p
end

# tiny bench method with nice printing,
# runs multiple tests
def mbench(n, runs, benches)
  label_width = benches.keys.max_by(&:length).length+1
  measures = []
  benches.each do |label, b|
    m, s = *bench(n,runs,&b)
    p = (100.0*s)/m
    measures << [label, m]
    printf "%-*s ø %fms (%.1f%%)\n", label_width, "#{label}:", m, p
  end

  measures.sort_by! { |l,m| m }
  rel = measures.first.last
  puts measures.map { |l,m| sprintf "%s: %.1f", l, m/rel }.join(', ')
  nil
end

# Require current project
$: << File.expand_path('.')

if File.directory? "lib"
  $:.unshift File.expand_path("./lib")
  $:.unshift File.expand_path("./ext") if File.directory? "ext"

  begin
    require File.basename(Dir.pwd)
  rescue LoadError
  end
end
