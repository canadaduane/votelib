require 'rubygems'
require 'csv'

if ARGV.size == 0
  puts "Usage: mkgraph.rb [election.csv] [candidate-1] [candidate-2] ... [candidate-n]"
else
  $csvfile = ARGV[0]
  $options = ARGV[1..-1]
end

template = <<-EOF
  digraph g {
    graph [
      rankdir = "LR"
    ];

    node [
      shape = "Mrecord"
    ];
    
%s
    
%s
  }
EOF

def prefs_array(prefs)
  arr = []
  prefs.each_with_index do |pref, i|
    (arr[pref] ||= []) << $options[i]
  end
  arr.map{ |a| a || [] }
end

nodes = []
edges = []
CSV.foreach($csvfile, :headers => :first_row, :return_headers => false) do |row|
  if row["Options"]
    $options = row["Options"].split("|")
    next
  end
  if row["DelegateID"]
    edges << %{    #{row["ID"]} -> #{row["DelegateID"]}}
  end
  if row["Preferences"]
    tr = %{<tr><td align="left">%s</td></tr>}
    prefs = row["Preferences"].split("|").map{ |x| x.to_i }
    pref_rows = prefs_array(prefs).map{ |p| tr % p.join(", ") }.join("")
    nodes << <<-ROW
    #{row["ID"]} [
      label = <<table border="0" cellborder="0" cellpadding="3" bgcolor="white"><tr><td bgcolor="black" align="center" colspan="2"><font color="white">#{row["Name"]}</font></td></tr>#{pref_rows}</table>>
    ];
    ROW
  else
    nodes << <<-ROW
    #{row["ID"]} [
      label = <<table border="0" cellborder="0" cellpadding="3" bgcolor="black"><tr><td bgcolor="black" align="center" colspan="2"><font color="white">#{row["Name"]}</font></td></tr></table>>
      style = "filled"
      fillcolor = "black"
    ];
    ROW
  end
end

puts(template % [nodes.join("\n"), edges.join("\n")])