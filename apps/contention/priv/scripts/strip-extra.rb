#!/usr/bin/env ruby

Dir["*.svg"].each do |filename|
  content = File.binread(filename)
  a = content.index('<g fill-opacity=".3">') || content.index('<g style="fill:rgb(0%,0%,0%);fill-opacity:0.3;">')
  next if a.nil?
  b = content.index('</g>', a)
  next if b.nil?
  File.binwrite(filename, [content.byteslice(0, a), content.byteslice(b + 4, content.bytesize)].join)
  puts "strip #{filename}"
end

exit(0)
