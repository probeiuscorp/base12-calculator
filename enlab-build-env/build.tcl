# if { [info exists ::env(PROJECT)] } {
#     set project_name $env(PROJECT)
# } else {
#     set path [lrange [file split [pwd] ] end end]
#     regexp {[^[:digit:]_].*} $path project_name    
# }

# if { [info exists ::env(TOP)] } {
#     set top $env(TOP)
# } else if { [file exists src/top.v] } {
set project_name Basys3_Master
set top top
# } else {
#     set top $project_name
# }

puts "project: $project_name"
puts "top:     ${top}.v"

# Load sources
read_verilog [ glob src/*.v ]
read_xdc ${project_name}.xdc

# Run Synthesis

synth_design -top ${top} -part xc7a35tcpg236-1
write_verilog -force post_synth.v

# Implement (optimize, place, route)
opt_design
place_design
route_design


# Generate Reports
report_timing_summary -file post_route_timing.rpt
report_utilization -file post_route_utilization.rpt

# Make bitstream
write_bitstream -force ${project_name}.bit
