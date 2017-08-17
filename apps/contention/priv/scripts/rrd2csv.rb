#!/usr/bin/env ruby

require 'csv'
require 'ox'

if ARGV.length != 1
  $stderr.puts "usage: #{$PROGRAM_NAME} FILENAME"
  exit(1)
end

filename = ARGV[0]
rrdfile = "#{filename}.rrd"
csvfile = "#{filename}.csv"

if not File.file?(rrdfile)
  $stderr.puts "FILENAME #{rrdfile.inspect} does not exist"
  exit(1)
end

xml = `rrdtool dump #{rrdfile}`
res = $?
if not res.success?
  $stderr.puts "Error running 'rrdtool dump #{rrdfile}'\n#{xml}"
  exit(res.exitstatus)
end

doc = Ox.parse(xml)
datasets = []
rows = []
read_database = ->(database) do
  i = 0
  loop do
    comment = database.nodes[i]
    break if comment.nil?
    row = database.nodes[i + 1]
    if comment.is_a?(Ox::Comment) and row.is_a?(Ox::Element) and row.value == 'row'
      timestamp = comment.value.split(' / ')[1].to_i
      values = []
      row.nodes.each do |v|
        raise "row has non-value elements" if v.value != 'v'
        value = v.text.strip
        if value == 'NaN'
          value = nil
        else
          value = value.to_f
        end
        values.push(value)
      end
      if not values.all?(&:nil?)
        values.unshift(timestamp)
        rows.push(values)
      end
    end
    i += 1
  end
end
doc.root.nodes.each do |node|
  case node.value
  when 'ds'
    ds = node
    ds.nodes.each do |node|
      case node.value
      when 'name'
        datasets.push(node.text.strip)
      end
    end
  when 'rra'
    rra = node
    rra.nodes.each do |node|
      case node.value
      when 'database'
        read_database.(node)
      end
    end
  end
end

CSV.open(csvfile, 'wb') do |csv|
  datasets.unshift('timestamp')
  csv << datasets
  rows.each do |row|
    csv << row
  end
end

$stdout.puts "wrote to #{csvfile.inspect}"
exit(0)
