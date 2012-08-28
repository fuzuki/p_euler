
def pe11(x,y,mx,my,data)
  prod = 1
  for i in 0..3
    prod *= data[y+(my*i)][x+(mx*i)]
  end
  return prod
end

data = []

File::open("pe11.dat") {|f|
  f.each {|line| data << line.chomp.split(/ /).map{|s|s.to_i}}
}

max = 0

for y in 0..(data.size() -1)
  for x in 0..(data[y].size() -1)
    if (x + 3) < data[y].size()
      tmp = pe11(x,y,1,0,data)
      if max < tmp
        max = tmp
      end
    end
    if ((x + 3) < data[y].size())&&((y + 3) < data.size())
      tmp = pe11(x,y,1,1,data)
      if max < tmp
        max = tmp
      end
    end
    if (y + 3) < data.size()
      tmp = pe11(x,y,0,1,data)
      if max < tmp
        max = tmp
      end
    end
    if ((x - 3) >= 0)&&((y + 3) < data.size())
      tmp = pe11(x,y,-1,1,data)
      if max < tmp
        max = tmp
      end
    end
  end
end

puts max
