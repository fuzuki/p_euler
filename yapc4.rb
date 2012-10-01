def prime?(n,plis)
  plis.each{|x|
    if n % x == 0
      return false
    elsif x * x > n
      return true
    end
  }
  return true
end

cnt = 2
sum = 2 + 3
i = 1
plis = [3]
while true
  pre = 6 * i - 1
  pos = 6 * i + 1
  if prime?(pre,plis)
    sum += pre
    plis << pre
    cnt += 1
  end
  if prime?(pos,plis)
    sum += pos
    plis << pos
    cnt += 1
  end
  if cnt >= 10000
    if cnt > 10000
      sum -= pos
    end
    break
  end
  i += 1
end
p sum
