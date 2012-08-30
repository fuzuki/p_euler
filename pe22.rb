def score(name)
  l = name.bytes.to_a.map{|c| c - "A".bytes.to_a[0] + 1 }
  sum = 0
  l.each{|x| sum += x}
  sum
end

open("names.txt") {|f|
  l = f.read.split(/,/).sort.map{|s| score(s.gsub(/"/,""))}
  for i in 1..(l.size())
    l[i-1] = l[i-1] * i
  end
  sum = 0
  l.each{|x| sum += x}
  puts sum
}
