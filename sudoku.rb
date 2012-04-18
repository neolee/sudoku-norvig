#!/usr/bin/env ruby

require "enumerator"

def all(seq)
  for e in seq do return false if not e end
  return true
end

def some(seq)
  for e in seq do return e if e end
  return false
end

def deep_copy(obj)
  return Marshal.load(Marshal.dump(obj))
end

def cross(a, b)
  return a.map{ |x| b.map{ |y| :"#{x}#{y}" } }.flatten
end

# Make yucky globals
$rows = ("A".."I").to_a
$cols = (1..9).to_a
$digits = (1..9).to_a
$squares = cross($rows, $cols)
$unit_list = [], $units = {}, $peers = {}

col_units = $cols.map{ |col| cross($rows, [col]) }
row_units = $rows.map{ |row| cross([row], $cols) }
box_units = []

$rows.each_slice(3) do |rs|
  $cols.each_slice(3) do |cs| 
    box_units << cross(rs, cs) 
  end
end

$unit_list = col_units + row_units + box_units

for s in $squares do
  $units[s] = $unit_list.select{ |u| u.include?(s) }
end

for s in $squares do
  $peers[s] = $units[s].flatten.uniq.select{ |s2| s2 != s }
end

def parse_grid(grid_str)
  grid = grid_str.split(//).map{ |s| s.to_i }
  values = {}
  for s in $squares do values[s] = deep_copy($digits) end

  for s, d in $squares.zip(grid) do
    return false if $digits.include?(d) and not assign(values, s, d)
  end
  
  return values
end

def assign(values, s, d)
  other_ds = values[s].select{ |d2| d2 != d }
  return values if all(other_ds.map{ |d2| eliminate(values, s, d2) })
  return false
end

def eliminate(values, s, d)
  return values if not values[s].include?(d)
  values[s].delete(d)
  return false if values[s].size == 0
  return false if values[s].size == 1 and
    not all($peers[s].map{ |s2| eliminate(values, s2, values[s][0]) })

  for u in $units[s] do
    dplaces = u.select{ |s| values[s].include?(d) }
    return false if dplaces.size == 0
    return false if dplaces.size == 1 and not assign(values, dplaces[0], d)
  end

  return values
end

def display(values)
  max_size = $squares.map{ |s| values[s].size }.max + 1
  line = (["-" * max_size * 3] * 3).join("+")

  for r in $rows do
    for c in $cols do
      values_str = values[:"#{r}#{c}"].join
      print values_str + (" " * (max_size - values_str.size))
      print "|" if c == 3 or c == 6
    end

    puts ""
    puts line if r == "C" or r == "F"
  end
end

def search(values)
  return false if not values
  return values if all($squares.map{ |s| (values[s].size == 1) })
  unsolved_squares = $squares.select{ |s| values[s].size > 1 }
  s = unsolved_squares.sort_by{ |s| values[s].size }.first
  return some(values[s].map{ |d| search(assign(deep_copy(values), s, d)) })
end

def solve(grid)
  return search(parse_grid(grid))
end

# Main
file = ARGV[0]
grid_str = File.read(file).gsub(/\n/, "")
display(solve(grid_str))
