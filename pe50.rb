require 'prime'

def pe50_1(n)
  primes = []
  Prime.each(n) do |prime|
    primes << prime
  end
  maxp = 0
  maxlen = 0
  primes.each do |prime|
    i = 0
    while(primes[i] < prime)
      sum = 0
      j = 0
      len = 0
      while(sum < prime)
        sum += primes[i+j]
        j += 1
      end
      if sum == prime
        len = j - i
        if len > maxlen
          maxlen = len
          maxp = prime
        end
      end
      i += 1
    end
  end
  maxp
end

def pe50_2(n)
  primes = []
  Prime.each(n) do |prime|
    primes << prime
  end
  maxp = 0
  maxlen = 0
  for i in 0..primes.size
    sum = 0
    j = 0
    while(sum < n && (i+j) < primes.size)
      sum += primes[i + j]
      if(Prime.prime?(sum))
        len = j - i + 1
        if(len > maxlen)
          maxlen = len
          maxp = sum
        end
      end
      j += 1
    end
  end
  maxp
end

p pe50_2(1000000)
