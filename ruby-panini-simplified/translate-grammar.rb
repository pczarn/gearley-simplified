require 'pry'
require_relative 'generator'

def cargo_toml filebase
    <<-TOML
[package]
name = "#{filebase}"
version = "0.1.0"
edition = "2021"

[lib]
name = "#{filebase}"

[dependencies]
logos = "0.12"
gearley-simplified = { path = "../../../lib-rust", version = "0.1" }
    TOML
end

output_dir = File.join(__dir__, "output")
grammars_dir = File.join(__dir__, "grammars")
p __dir__, output_dir
`mkdir #{output_dir}`

Dir["#{grammars_dir}/*.bootstrap.txt"].each do |path|
    grammar = File.read path
    filebase = path[/#{Regexp.quote(grammars_dir)}\/(.*)\.bootstrap\.txt\Z/, 1]
    grammar_output_dir = File.join(output_dir, filebase)
    puts "cd #{output_dir}; cargo init #{filebase} --lib"
    `cd #{output_dir}; cargo init #{filebase} --lib`
    File.write "#{grammar_output_dir}.rb", RbGenerator.new(grammar).generate
    File.write "#{grammar_output_dir}/src/lib.rs", RsGenerator.new(grammar).generate
    File.write "#{grammar_output_dir}/Cargo.toml", cargo_toml(filebase)
    File.write "#{grammar_output_dir}/.gitignore", "/target/\nCargo.lock\n"
    `cd #{grammar_output_dir}; cargo fmt`
end
