d1 = ["","one","two","three","four","five","six","seven","eight","nine"]
d2 = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
d3 = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

l = []
for i in 1..1000
  if i == 1000
    l << "onethousand"
    next
  end
  numstr = ""
  n = i % 100
  if (i / 100) > 0
    numstr += d1[i/100] + "hundred"
    if n > 0
      numstr += "and"
    else
      l << numstr
      next
    end
  end
  
  if n < 10
    numstr += d1[n]
  elsif n < 20
    numstr += d2[n%10]
  else
    numstr += d3[n/10] + d1[n%10]
  end
  l << numstr
end

#p l
sum = 0
l.map{|s| s.size}.each{|x| sum += x}
puts sum
