require 'rake'
require 'rake/clean'

# Application Configuration
APPNAME = "yawsapp"
VSN = "0.1"

# Run parameters
START_MODULE = "#{APPNAME}"
TEST_MODULE = "test_myerlang"
MNESIA_DIR = "tmp"
MNESIA = "-mnesia dir '"#{MNESIA_DIR}"'"
YAWS = "-yaws embedded true"
# Emulator flags
THREADS = 1
WARNING_LEVEL = "w"


# No Need to change
if PLATFORM =~ /win32/
  PWD = `cd`.strip.gsub!(/\\/, "/")
else
  PWD = `pwd`.strip
end

print PWD

INCLUDE = "include"
ERLC_FLAGS = [
		"-I#{INCLUDE}",
		"+warn_unused_vars",
		"+warn_unused_import",
#		"-Ddebug",
#		"-Dexport_all"
#		"+trace",
	].join(" ")


SRC = FileList['src/**/*.erl']
OBJ = SRC.pathmap("%{src,ebin}X.beam")
APPFILE = "ebin/#{APPNAME}.app"
MODULES = SRC.pathmap("%n")
CLEAN.include(['**/*.dump'])
CLOBBER.include(['**/*.beam'])

directory 'ebin'

rule "#{APPNAME}.app"  =>  FileList['src/**/*.app.src'] do |t|
  if t.source
    file = File.new(t.source)
    lines = file.readlines
    file.close
    lines.each do |line|
      line.gsub!(/%VSN%/, VSN)
      line.gsub!(/%MODULES%/, MODULES.join(","))
      line.gsub!(/%WORKING_DIR%/, PWD)
    end
    file = File.new(t.name,'w')
    lines.each do |line|
      file.write(line)
    end
    file.close
  end
end

rule ".beam" =>  ["%{ebin,src}X.erl"] do |t|
  sh "erlc -pa ebin -W #{ERLC_FLAGS} -o ebin #{t.source}"
end

desc "Compile all"
task :compile => ['ebin'] + OBJ + [:appfile]

desc "Compile appfile"
task :appfile => APPFILE

desc "Open up a shell"
task :shell => [:compile] do
    sh("erl -sname #{START_MODULE} -pa #{PWD}/ebin")
end

desc "Open up a shell and run #{START_MODULE}:start()" 
task :run => [:compile] do
  sh("erl +A #{THREADS} +W #{WARNING_LEVEL} -boot start_sasl -sname #{START_MODULE} -pa #{PWD}/ebin -run #{START_MODULE} #{YAWS} #{MNESIA}")
end

desc "Run Unit Tests" 
task :test do
  sh("erl -noshell -s #{TEST_MODULE} test -s init stop")
end


desc "Generate Documentation"
task :doc do
    sh("cd doc && erl -noshell -run edoc files ../#{SRC.join(" ../")} -run init stop")
end

task :run_tests => [:compile] do
  puts "Modules under test:"
  OBJ.each do |obj|
    obj[%r{.*/(.*).beam}]
    mod = $1
    test_output = `erl -pa ebin -run #{mod} test -run init stop`

    if /\*failed\*/ =~ test_output
      test_output[/(Failed.*Aborted.*Skipped.*Succeeded.*$)/]
    else
      test_output[/1>\s*(.*)\n/]
    end

    puts "#{mod}: #{$1}"
  end
end



task :default => :compile
